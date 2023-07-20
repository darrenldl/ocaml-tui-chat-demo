type t

type payload = [
  | `Heartbeat
  | `Ack
  | `Data of string
  | `User_name_register of string
  | `Register_okay of string
  | `Register_fail of string
]

val make_heartbeat : id:int -> t

val make_ack : id:int -> t

val make_data : id:int -> string -> t

val make_user_name_register : id:int -> user_name:string -> t

val make_register_okay : id:int -> user_name:string -> t

val make_register_fail : id:int -> user_name:string -> t

val id : t -> int

val payload : t -> payload

val parser : t Eio.Buf_read.parser

val write : Eio.Buf_write.t -> t -> unit
