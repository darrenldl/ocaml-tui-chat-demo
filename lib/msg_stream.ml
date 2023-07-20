module Ingress_ = struct
  module Read = Eio.Buf_read

  type error = [ `Msg of string | `Timeout | `Eol | `Old_outstanding_ack of string ]

  type t = (Msg.t, error) result Eio.Stream.t

  let make ~sw ~clock (source : Eio.Flow.source) : t =
    let stream = Eio.Stream.create Int.max_int in
    let rec aux seq =
      let msg_and_seq =
        Eio.Fiber.first
          (fun () -> Eio.Time.Mono.sleep clock Params.idle_timeout_s; Some (Error `Timeout))
          (fun () ->
             match seq () with
             | Seq.Nil -> None
             | Seq.Cons (x, rest) -> Some (Ok (x, rest))
          )
      in
      match msg_and_seq with
      | None -> ()
      | Some x -> (
          match x with
          | Ok (msg, rest) -> (
              Eio.Stream.add stream (Ok msg);
              aux rest
            )
          | Error x -> Eio.Stream.add stream (Error x)
        )
    in
    Eio.Fiber.fork ~sw (fun () ->
        match Read.parse ~max_size:Int.max_int Read.(map aux (seq Msg.parser)) source with
        | Ok () -> Eio.Stream.add stream (Error `Eol)
        | Error (`Msg msg) -> Eio.Stream.add stream (Error (`Msg msg))
        | exception _ -> Eio.Stream.add stream (Error `Eol)
      );
    stream

  let take (t : t) : (Msg.t, error) result =
    Eio.Stream.take t

  let add_error (t : t) x =
    Eio.Stream.add t (Error x)
end

module Egress_ = struct
  module Write = Eio.Buf_write

  type t = Msg.t option Eio.Stream.t

  let make ~sw (sink : Eio.Flow.sink ) : t =
    let stream = Eio.Stream.create Int.max_int in
    let rec aux () =
      match Eio.Stream.take stream with
      | None -> ()
      | Some msg -> (
          match
            Write.with_flow sink (fun w ->
                Msg.write w msg;
              )
          with
          | () -> aux ()
          | exception _ -> ()
        )
    in
    Eio.Fiber.fork ~sw aux;
    stream

  let send (t : t) msg =
    Eio.Stream.add t (Some msg)

  let close (t : t) =
    Eio.Stream.add t None
end

module H = Kcas_data.Hashtbl

type t = {
  stop : bool Atomic.t;
  flow : Eio.Flow.two_way;
  clock : Eio.Time.Mono.t;
  ingress_low : Ingress_.t;
  ingress_high : (Msg_info.t, Ingress_.error) result Eio.Stream.t;
  egress : Egress_.t;
  egress_counter : int Atomic.t;
  outstanding_acks : (int, Mtime.t * string) H.t;
}

type error = Ingress_.error

let close (t : t) =
  Atomic.set t.stop true;
  Egress_.close t.egress;
  Eio.Stream.add t.ingress_high (Error `Eol);
  try Eio.Flow.shutdown t.flow `All with _ -> ()

let send_heartbeat (t : t) =
  let id = Atomic.fetch_and_add t.egress_counter 1 in
  Egress_.send t.egress (Msg.make_heartbeat ~id)

let take (t : t) =
  Eio.Stream.take t.ingress_high

let record_outstanding_ack (t : t) ~id data =
  let now = Eio.Time.Mono.now t.clock in
  H.add t.outstanding_acks id (now, data)

let acknowledge (t : t) ~id =
  Egress_.send t.egress (Msg.make_ack ~id)

let send_data (t : t) (data : string) =
  let id = Atomic.fetch_and_add t.egress_counter 1 in
  record_outstanding_ack t ~id data;
  Egress_.send t.egress (Msg.make_data ~id data)

let send_user_name_reg (t : t) ~user_name =
  let id = Atomic.fetch_and_add t.egress_counter 1 in
  Egress_.send t.egress (Msg.make_user_name_register ~id ~user_name)

let send_reg_okay (t : t) ~user_name =
  let id = Atomic.fetch_and_add t.egress_counter 1 in
  Egress_.send t.egress (Msg.make_register_okay ~id ~user_name)

let send_reg_fail (t : t) ~user_name =
  let id = Atomic.fetch_and_add t.egress_counter 1 in
  Egress_.send t.egress (Msg.make_register_fail ~id ~user_name)

let closed (t : t) =
  Atomic.get t.stop

let make ~sw ~clock (flow : < Eio.Flow.two_way; Eio.Flow.close >) : t =
  let flow = (flow :> Eio.Flow.two_way) in
  let ingress_low = Ingress_.make ~sw ~clock (flow :> Eio.Flow.source) in
  let ingress_high = Eio.Stream.create Int.max_int in
  let egress = Egress_.make ~sw (flow :> Eio.Flow.sink) in
  let outstanding_acks = H.create () in
  let t =
    {
      stop = Atomic.make false;
      flow;
      clock;
      ingress_low;
      ingress_high;
      egress;
      egress_counter = Atomic.make 0;
      outstanding_acks;
    }
  in
  let rec heartbeat () =
    if not (Atomic.get t.stop) then (
      Eio.Time.Mono.sleep clock 2.0;
      Logger.log_debug (fun () -> [ "Sending heartbeat" ]);
      send_heartbeat t;
      heartbeat ()
    )
  in
  let rec check_outstanding_acks () =
    if not (Atomic.get t.stop) then (
      Eio.Time.Mono.sleep clock 1.0;
      let now = Eio.Time.Mono.now t.clock in
      let old_outstanding_ack = ref None in
      H.iter (fun _id (timestamp, data) ->
          let diff = Mtime.span timestamp now in
          if Mtime.Span.compare diff Params.ack_old_threshold >= 0 then (
            old_outstanding_ack := Some data
          )
        ) outstanding_acks;
      (match !old_outstanding_ack with
       | None -> check_outstanding_acks ()
       | Some data -> (
           Logger.log_debug (fun () -> [ "Old outstanding ack detected" ]);
           Ingress_.add_error t.ingress_low (`Old_outstanding_ack data);
           close t;
         )
      )
    )
  in
  let rec forward_ingress_msg () =
    if Atomic.get t.stop then (
      Eio.Stream.add t.ingress_high (Error `Eol);
      close t
    ) else (
      match Ingress_.take t.ingress_low with
      | Error x -> (
          Eio.Stream.add t.ingress_high (Error x);
          close t
        )
      | Ok msg -> (
          (match Msg.payload msg with
           | `Ack -> (
               match H.find_opt t.outstanding_acks (Msg.id msg) with
               | None -> ()
               | Some (timestamp, data) -> (
                   let now = Eio.Time.Mono.now t.clock in
                   let rtt_ns = Mtime.span now timestamp
                                |> Mtime.Span.to_uint64_ns
                   in
                   H.remove t.outstanding_acks (Msg.id msg);
                   Eio.Stream.add t.ingress_high (Ok (`Ack { Msg_info.rtt_ns; data }))
                 )
             )
           | `Data _ as x -> (
               acknowledge t ~id:(Msg.id msg);
               Eio.Stream.add t.ingress_high (Ok (x :> Msg_info.t))
             )
           | `Heartbeat
           | `User_name_register _
           | `Register_okay _
           | `Register_fail _ as x -> Eio.Stream.add t.ingress_high (Ok (x :> Msg_info.t))
          );
          forward_ingress_msg ()
        )
    )
  in
  Eio.Fiber.fork ~sw heartbeat;
  Eio.Fiber.fork ~sw check_outstanding_acks;
  Eio.Fiber.fork ~sw forward_ingress_msg;
  t
