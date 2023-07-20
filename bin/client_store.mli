open Chat_lib

type t

val make : unit -> t

type info = {
  user_name : string option;
  sockaddr : string;
  msg_stream : Msg_stream.t;
}

val info_of_user_name : t -> user_name:string -> info option

val info_of_sockaddr : t -> sockaddr:string -> info option

type register_result = [
  | `Success
  | `Already_used
  | `Already_shutdown
]

val register :
  t ->
  user_name:string option ->
  sockaddr:string ->
  Msg_stream.t ->
  register_result

val deregister_by_user_name : t -> user_name:string -> unit

val deregister_by_sockaddr : t -> sockaddr:string -> unit

val shutdown : t -> unit
