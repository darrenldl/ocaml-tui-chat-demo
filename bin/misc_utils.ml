let ipaddr_of_string s =
  match Unix.inet_addr_of_string s with
  | x -> Some (Eio_unix.Net.Ipaddr.of_unix x)
  | exception _ -> None

let display_rtt_of_rtt_ns (rtt_ns : int64) =
  Fmt.str "%.3fs" (Int64.to_float rtt_ns /. 1_000_000_000.0)
