open Cmdliner

let cmd ~env : int Cmd.t =
  let doc = "Chat server and client" in
  let version = Version_string.s in
  Cmd.group (Cmd.info "chat" ~version ~doc)
    [
      Client.cmd ~env;
      Server.cmd ~env;
    ]

let () = Eio_main.run (fun env ->
    exit (Cmd.eval' (cmd ~env))
  )
