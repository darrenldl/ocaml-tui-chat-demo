open Chat_lib
open Option_syntax

type t = {
  lock : Eio.Mutex.t;
  mutable shutdown : bool;
  mutable user_name_of_sockaddr : string String_map.t;
  mutable sockaddr_of_user_name : string String_map.t;
  mutable msg_stream_of_sockaddr : Msg_stream.t String_map.t;
}

let make () : t =
  {
    lock = Eio.Mutex.create ();
    shutdown = false;
    user_name_of_sockaddr = String_map.empty;
    sockaddr_of_user_name = String_map.empty;
    msg_stream_of_sockaddr = String_map.empty;
  }

type info = {
  user_name : string option;
  sockaddr : string;
  msg_stream : Msg_stream.t;
}

let info_of_sockaddr_internal (t : t) ~sockaddr : info option =
  let+ msg_stream = String_map.find_opt sockaddr t.msg_stream_of_sockaddr in
  {
    user_name = String_map.find_opt sockaddr t.user_name_of_sockaddr;
    sockaddr;
    msg_stream;
  }

let info_of_user_name (t : t) ~user_name : info option =
  Eio.Mutex.use_ro t.lock (fun () ->
      let* sockaddr = String_map.find_opt user_name t.sockaddr_of_user_name in
      info_of_sockaddr_internal t ~sockaddr
    )

let info_of_sockaddr (t : t) ~sockaddr : info option =
  Eio.Mutex.use_ro t.lock (fun () ->
      info_of_sockaddr_internal t ~sockaddr
    )

type register_result = [
  | `Success
  | `Already_used
  | `Already_shutdown
]

let register
    (t : t)
    ~user_name
    ~sockaddr
    (msg_stream : Msg_stream.t)
  : register_result =
  Eio.Mutex.use_rw ~protect:true t.lock (fun () ->
      if t.shutdown then
        `Already_shutdown
      else (
        match user_name with
        | None -> (
            t.msg_stream_of_sockaddr <- String_map.add sockaddr msg_stream t.msg_stream_of_sockaddr;
            `Success
          )
        | Some user_name -> (
            match String_map.find_opt user_name t.sockaddr_of_user_name with
            | None -> (
                t.user_name_of_sockaddr <- String_map.add sockaddr user_name t.user_name_of_sockaddr;
                t.sockaddr_of_user_name <- String_map.add user_name sockaddr t.sockaddr_of_user_name;
                t.msg_stream_of_sockaddr <- String_map.add sockaddr msg_stream t.msg_stream_of_sockaddr;
                `Success
              )
            | Some _ -> `Already_used
          )
      )
    )

let deregister_internal (t : t) ~sockaddr =
  let sockaddr_of_user_name =
    match String_map.find_opt sockaddr t.user_name_of_sockaddr with
    | None -> t.sockaddr_of_user_name
    | Some user_name -> String_map.remove user_name t.sockaddr_of_user_name
  in
  t.user_name_of_sockaddr <- String_map.remove sockaddr t.user_name_of_sockaddr;
  t.sockaddr_of_user_name <- sockaddr_of_user_name;
  t.msg_stream_of_sockaddr <- String_map.remove sockaddr t.msg_stream_of_sockaddr

let deregister_by_user_name
    (t : t)
    ~user_name
  =
  Eio.Mutex.use_rw ~protect:true t.lock (fun () ->
      match String_map.find_opt user_name t.sockaddr_of_user_name with
      | None -> ()
      | Some sockaddr -> deregister_internal t ~sockaddr
    )

let deregister_by_sockaddr
    (t : t)
    ~sockaddr
  =
  Eio.Mutex.use_rw ~protect:true t.lock (fun () ->
      deregister_internal t ~sockaddr
    )

let shutdown (t : t) =
  Eio.Mutex.use_rw ~protect:true t.lock (fun () ->
      t.shutdown <- true;
      String_map.iter (fun _sockaddr msg_stream ->
          Msg_stream.close msg_stream
        ) t.msg_stream_of_sockaddr
    )
