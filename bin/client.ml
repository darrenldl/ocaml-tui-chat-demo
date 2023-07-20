open Cmdliner
open Chat_lib

module Write = Eio.Buf_write

let debug_arg =
  let doc =
    Fmt.str "Display debug info."
  in
  Arg.(value & flag & info [ "debug" ] ~doc)

let host_arg =
  let doc =
    "Server to connect to."
  in
  Arg.(value & opt string Params.client_default_dest_host & info [ "host" ] ~doc)

let port_arg =
  let doc =
    "Port of server to use."
  in
  Arg.(value & opt int Params.client_default_dest_port & info [ "port" ] ~doc)

let user_name_arg =
  let doc =
    "User name."
  in
  Arg.(value & opt string "" & info [ "user-name" ] ~doc)

module Read = Eio.Buf_read

module Ui = struct
  let main ~logs ~on_receive_input =
    Ui_base.Chat_and_log_pane.main ~logs ~on_receive_input
end

let run ~(env : Eio_unix.Stdenv.base) debug host port user_name : int =
  Params.debug := debug;
  let clock = Eio.Stdenv.clock env in
  let logs = Lwd.var [] in
  Eio.Switch.run (fun sw ->
      Logger.init ~sw ~clock ~debug ~logs;
      let service = string_of_int port in
      let clock = Eio.Stdenv.mono_clock env in
      let timeout = Eio.Time.Timeout.seconds clock Params.initial_connect_timeout_s in
      let net = Eio.Stdenv.net env in
      let quit = Lwd.var false in
      let term = Notty_unix.Term.create () in
      Ui_base.Vars.term := Some term;
      let display ~msg_stream =
        let rec aux () =
          match Msg_stream.take msg_stream with
          | Ok info -> (
              (match info with
               | `Heartbeat -> Logger.log_debug (fun () -> [ Fmt.str "Heartbeat" ])
               | `Ack { rtt_ns; data } -> (
                   let display_rtt = Misc_utils.display_rtt_of_rtt_ns rtt_ns in
                   Logger.log (fun () -> [ Fmt.str "(ACK RTT: %s) %s" display_rtt data ])
                 )
               | `Data data -> Logger.log (fun () -> [ Fmt.str "server: %s" data ])
               | `User_name_register _ -> ()
               | `Register_fail user_name -> (
                   Logger.log (fun () -> [ Fmt.str "User name %s registration failed" user_name ])
                 )
               | `Register_okay _ -> (
                   Logger.log (fun () -> [ Fmt.str "User name %s registration was successful" user_name ])
                 )
              );
              aux ()
            )
          | Error err -> (
              (match err with
               | `Timeout -> Logger.log (fun () -> [ Fmt.str "Timed out" ])
               | `Eol -> Logger.log (fun () -> [ Fmt.str "Connection ended" ])
               | `Old_outstanding_ack s ->
                 Logger.log (fun () -> [ Fmt.str "Did not receive acknowledgement for:" ; s ])
               | `Msg s -> Logger.log (fun () -> [ Fmt.str "Error with message %s" s ])
              )
            )
        in
        aux ()
      in
      (match
         Eio.Net.with_tcp_connect ~timeout ~host ~service net (fun flow ->
             let msg_stream = Msg_stream.make ~sw ~clock flow in
             if user_name <> "" then (
               Msg_stream.send_user_name_reg msg_stream ~user_name
             );
             let prompt_label = "Chat" in
             let on_receive_input s =
               Msg_stream.send_data msg_stream s
             in
             Eio.Fiber.both
               (fun () ->
                  Ui_base.ui_loop ~quit (Ui.main ~logs ~on_receive_input ~prompt_label);
                  Msg_stream.close msg_stream
               )
               (fun () -> display ~msg_stream; Lwd.set quit true);
           )
       with
       | () -> ()
       | exception _ -> Fmt.pr "@[<v>Failed to connect to %s:%d@,@]" host port
      );
      Logger.shutdown ();
    );
  0

let cmd ~env =
  let doc = "Client mode" in
  let version = Version_string.s in
  Cmd.v (Cmd.info "client" ~version ~doc)
    Term.(const (run ~env)
          $ debug_arg
          $ host_arg
          $ port_arg
          $ user_name_arg)
