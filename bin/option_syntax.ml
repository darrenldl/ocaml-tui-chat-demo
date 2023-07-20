let ( let+ ) = (fun x f -> Option.map f x)

let ( let* ) = Option.bind
