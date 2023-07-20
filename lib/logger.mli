val init :
  sw:Eio.Switch.t ->
  clock:Eio.Time.clock ->
  debug:bool ->
  logs:Notty.image list Lwd.var ->
  unit

val shutdown : unit -> unit

val log : (unit -> string list) -> unit

val log_debug : (unit -> string list) -> unit
