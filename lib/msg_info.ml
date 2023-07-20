type ack = { rtt_ns : int64; data : string }

type t = [
  | `Heartbeat
  | `Ack of ack
  | `Data of string
  | `User_name_register of string
  | `Register_okay of string
  | `Register_fail of string
]
