open Cmdliner
open Chat_lib

module Write = Eio.Buf_write

let debug_arg =
  let doc =
    Fmt.str "Display debug info."
  in
  Arg.(value & flag & info [ "debug" ] ~doc)

let addr_arg =
  let doc =
    "Address to listen on."
  in
  Arg.(value & opt string Params.server_default_listen_host & info [ "addr" ] ~doc)

let port_arg =
  let doc =
    "Port to use."
  in
  Arg.(value & opt int Params.client_default_dest_port & info [ "port" ] ~doc)

let start_client_handler ~client_sockaddr msg_stream client_store =
  let rec aux () =
    let display_name =
      match Client_store.info_of_sockaddr client_store ~sockaddr:client_sockaddr with
      | None -> client_sockaddr
      | Some { user_name; _ } -> (
          match user_name with
          | None -> client_sockaddr
          | Some user_name -> user_name
        )
    in
    match Msg_stream.take msg_stream with
    | Ok info -> (
        (match info with
         | `Heartbeat -> Logger.log_debug (fun () -> [ Fmt.str "<%s>: Heartbeat" display_name ])
         | `Ack { rtt_ns; data } -> (
             let display_rtt = Misc_utils.display_rtt_of_rtt_ns rtt_ns in
             Logger.log (fun () -> [ Fmt.str "(ACK RTT: %s) %s" display_rtt data ])
           )
         | `Data data -> Logger.log (fun () -> [ Fmt.str "<%s> says: %s" display_name data ])
         | `User_name_register user_name -> (
             let line0 = Fmt.str "<%s> tries to register user name: %s" client_sockaddr user_name in
             if String.contains user_name ' ' then (
               Logger.log (fun () -> [ line0; Fmt.str "User name is invalid: Contains space" ]);
               Msg_stream.send_reg_fail msg_stream ~user_name;
             ) else (
               match
                 Client_store.register
                   client_store
                   ~user_name:(Some user_name)
                   ~sockaddr:client_sockaddr
                   msg_stream
               with
               | `Already_shutdown -> ()
               | `Already_used -> (
                   Logger.log (fun () -> [ line0; Fmt.str "User name has been taken" ]);
                   Msg_stream.send_reg_fail msg_stream ~user_name;
                 )
               | `Success -> (
                   Logger.log (fun () -> [ line0; Fmt.str "Registration was successful" ]);
                   Msg_stream.send_reg_okay msg_stream ~user_name;
                 )
             )
           )
         | `Register_fail _ -> ()
         | `Register_okay _ -> ()
        );
        aux ()
      )
    | Error err -> (
        (match err with
         | `Timeout -> Logger.log (fun () -> [ Fmt.str "<%s>: Timed out" display_name ])
         | `Eol -> Logger.log (fun () -> [ Fmt.str "<%s>: Connection ended" display_name ])
         | `Old_outstanding_ack s ->
           Logger.log (fun () -> [ Fmt.str "Did not receive acknowledgement from <%s> for:" display_name; s ])
         | `Msg s -> Logger.log (fun () -> [ Fmt.str "<%s>: Error with message %s" display_name s ])
        );
        Client_store.deregister_by_sockaddr client_store ~sockaddr:client_sockaddr
      )
  in
  match
    Client_store.register
      client_store
      ~user_name:None
      ~sockaddr:client_sockaddr
      msg_stream
  with
  | `Success -> aux ()
  | `Already_used -> failwith "Unexpected case"
  | `Already_shutdown -> ()

module Ui = struct
  let main ~logs ~on_receive_input =
    Ui_base.Chat_and_log_pane.main ~logs ~on_receive_input
end

module Console_command = struct
  type t = {
    command : string;
    args : string list;
  }

  let of_string s : (t, string) result =
    match String.split_on_char ' ' s with
    | [] -> Error "Empty command"
    | command :: args -> Ok { command; args }
end

let run ~(env : Eio_unix.Stdenv.base) debug host port : int =
  let client_store = Client_store.make () in
  match Misc_utils.ipaddr_of_string host with
  | None -> (
      Printf.printf "Invalid host address: %S\n" host;
      1
    )
  | Some host -> (
      Params.debug := debug;
      let clock = Eio.Stdenv.clock env in
      let logs = Lwd.var [] in
      Eio.Switch.run (fun sw ->
          Logger.init ~sw ~clock ~debug ~logs;
          let clock = Eio.Stdenv.mono_clock env in
          let net = Eio.Stdenv.net env in
          (match
             Eio.Net.listen ~backlog:Params.server_conn_queue ~sw net
               (`Tcp (host, port))
           with
           | listening_socket -> (
               Logger.log (fun () -> [ Fmt.str "Press Esc to terminate" ]);
               Logger.log (fun () -> [ Fmt.str "Listening on %a:%d" Eio.Net.Ipaddr.pp host port ]);
               let quit = Lwd.var false in
               let client_dispatch_budget = Eio.Semaphore.make Params.concurrent_client_count in
               let rec dispatcher () =
                 let got_budget =
                   Eio.Fiber.first
                     (fun () -> Eio.Time.Mono.sleep clock 0.5; false)
                     (fun () -> Eio.Semaphore.acquire client_dispatch_budget; true)
                 in
                 if got_budget then (
                   let flow_and_client =
                     Eio.Fiber.first
                       (fun () -> Eio.Time.Mono.sleep clock 0.5; None)
                       (fun () -> Some (Eio.Net.accept ~sw listening_socket))
                   in
                   (match flow_and_client with
                    | None -> Eio.Semaphore.release client_dispatch_budget
                    | Some (flow, client) -> (
                        Eio.Fiber.fork ~sw (fun () ->
                            let client_sockaddr = Fmt.str "%a" Eio.Net.Sockaddr.pp client in
                            Logger.log (fun () -> [ Fmt.str "Received connection from %s" client_sockaddr ]);
                            let msg_stream = Msg_stream.make ~sw ~clock flow in
                            start_client_handler ~client_sockaddr msg_stream client_store;
                            Eio.Semaphore.release client_dispatch_budget;
                          )
                      )
                   );
                 );
                 if not (Lwd.peek quit) then (
                   dispatcher ()
                 )
               in
               let on_receive_input s =
                 match Console_command.of_string s with
                 | Error s -> Logger.log (fun () -> [ Fmt.str "Error: %s" s ])
                 | Ok { command; args } -> (
                     match command with
                     | "msg" -> (
                         match args with
                         | [] -> Logger.log (fun () -> [ Fmt.str "Error: Expected user name and message" ])
                         | [ _ ] -> Logger.log (fun () -> [ Fmt.str "Error: Empty message" ])
                         | user_name_or_sockaddr :: texts -> (
                             match Client_store.info_of_user_name client_store ~user_name:user_name_or_sockaddr with
                             | None -> (
                                 match Client_store.info_of_sockaddr client_store ~sockaddr:user_name_or_sockaddr with
                                 | None -> Logger.log (fun () -> [ Fmt.str "Error: No such user %s" user_name_or_sockaddr ])
                                 | Some { msg_stream; _ } ->
                                   Msg_stream.send_data msg_stream (String.concat " " texts)
                               )
                             | Some { msg_stream; _ } ->
                               Msg_stream.send_data msg_stream (String.concat " " texts)
                           )
                       )
                     | _ -> Logger.log (fun () -> [ Fmt.str "Error: Unrecognized command %s" command ])
                   )
               in
               let term = Notty_unix.Term.create () in
               Ui_base.Vars.term := Some term;
               let prompt_label = "Console" in
               Eio.Fiber.both
                 (fun () ->
                    Ui_base.ui_loop ~quit (Ui.main ~logs ~on_receive_input ~prompt_label);
                    Client_store.shutdown client_store
                 )
                 dispatcher;
             )
           | exception _ -> Fmt.pr "@[<v>Failed to bind to %a:%d@]" Eio.Net.Ipaddr.pp host port
          );
          Logger.shutdown ();
        );
      0
    )

let cmd ~env =
  let doc = "Server mode" in
  let version = Version_string.s in
  Cmd.v (Cmd.info "server" ~version ~doc)
    Term.(const (run ~env)
          $ debug_arg
          $ addr_arg
          $ port_arg)
