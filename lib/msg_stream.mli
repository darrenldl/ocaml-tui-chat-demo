type t

type error = [ `Msg of string | `Timeout | `Eol | `Old_outstanding_ack of string ]

val make : sw:Eio.Switch.t -> clock:Eio.Time.Mono.t -> < Eio.Flow.two_way; Eio.Flow.close > -> t

val take : t -> (Msg_info.t, error) result

val send_data : t -> string -> unit

val acknowledge : t -> id:int -> unit

val send_heartbeat : t -> unit

val send_user_name_reg : t -> user_name:string -> unit

val send_reg_okay : t -> user_name:string -> unit

val send_reg_fail : t -> user_name:string -> unit

val close : t -> unit

val closed : t -> bool
