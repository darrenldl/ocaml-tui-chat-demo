type t = {
  debug : bool;
  clock : Eio.Time.clock;
  stream : (Timedesc.timestamp * (unit -> string list)) option Eio.Stream.t;
}

let t : t option ref = ref None

let format = "{year}-{mon:0X}-{day:0X} {hour:0X}:{min:0X}:{sec:0X}{sec-frac:.3}"

let init ~sw ~clock ~debug ~(logs : Notty.image list Lwd.var) =
  let t' = { debug; clock; stream = Eio.Stream.create 1000 } in
  let rec aux () : unit =
    match Eio.Stream.take t'.stream with
    | None -> ()
    | Some (timestamp, work) -> (
        let lines = work () in
        let content_img =
          List.map (fun line ->
              match Notty.I.strf "%s" line with
              | x -> x
              | exception _ -> (
                  Hex.of_string line
                  |> Hex.hexdump_s ~print_row_numbers:true ~print_chars:true
                  |> CCString.lines
                  |> List.map (fun line -> Notty.I.strf "%s" line)
                  |> Notty.I.vcat
                )
            ) lines
        in
        let timestamp_img =
          Notty.I.strf "[%a] " Timedesc.Timestamp.(pp ~format ()) timestamp
        in
        let img =
          Notty.I.(timestamp_img <|> (vcat content_img))
        in
        let logs' = Lwd.peek logs in
        Lwd.set logs ((img :: logs') |> CCList.take 300);
        aux ()
      )
  in
  Eio.Fiber.fork ~sw aux;
  t := Some t'

let shutdown () =
  let t = Option.get !t in
  Eio.Stream.add t.stream None

let log work =
  let t = Option.get !t in
  let now = Timedesc.Timestamp.of_float_s (Eio.Time.now t.clock) in
  Eio.Stream.add t.stream (Some (now, work))

let log_debug work =
  let t = Option.get !t in
  if t.debug then log work
