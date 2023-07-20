type typ =
  | Heartbeat
  | Ack
  | Data
  | User_name_register
  | Register_fail
  | Register_okay

type t = {
  id : int;
  typ : typ;
  payload : string;
}

type payload = [
  | `Heartbeat
  | `Ack
  | `Data of string
  | `User_name_register of string
  | `Register_okay of string
  | `Register_fail of string
]

let make ~id typ payload =
  let id = id land 0xFFFF in
  { id; typ; payload }

let make_heartbeat ~id : t = make ~id Heartbeat ""

let make_ack ~id : t = make ~id Ack ""

let make_data ~id payload = make ~id Data payload

let make_user_name_register ~id ~user_name = make ~id User_name_register user_name

let make_register_okay ~id ~user_name = make ~id Register_okay user_name

let make_register_fail ~id ~user_name = make ~id Register_fail user_name

let id (t : t) : int =
  t.id

(*let data (t : t) : string option =
  match t.typ with
  | Data -> Some t.payload
  | _ -> None *)

let payload (t : t) : payload =
  match t.typ with
  | Heartbeat -> `Heartbeat
  | Ack -> `Ack
  | Data -> `Data t.payload
  | User_name_register -> `User_name_register t.payload
  | Register_okay -> `Register_okay t.payload
  | Register_fail -> `Register_fail t.payload

let int_of_typ (typ : typ) =
  let x =
    match typ with
    | Heartbeat -> 0x00
    | Ack -> 0x01
    | Data -> 0x02
    | User_name_register -> 0x03
    | Register_okay -> 0x04
    | Register_fail -> 0x05
  in
  assert (0x00 <= x && x <= 0xFF);
  x

let typ_of_int (x : int) =
  match x with
  | 0x00 -> Some Heartbeat
  | 0x01 -> Some Ack
  | 0x02 -> Some Data
  | 0x03 -> Some User_name_register
  | 0x04 -> Some Register_okay
  | 0x05 -> Some Register_fail
  | _ -> None

module Read = Eio.Buf_read

let parser : t Read.parser =
  let open Read.Syntax in
  let* id = Read.BE.uint16 in
  let* typ = Read.uint8 in
  match typ_of_int typ with
  | None -> raise (Failure "Invalid message type")
  | Some typ ->
    let* len = Read.BE.uint32 in
    let len = Int32.to_int len in
    let* payload = Read.take len in
    Read.return { id; typ; payload }

module Write = Eio.Buf_write

let write (b : Write.t) (t : t) : unit =
  Write.BE.uint16 b t.id;
  Write.uint8 b (int_of_typ t.typ);
  Write.BE.uint32 b (Int32.of_int (String.length t.payload));
  Write.string b t.payload
