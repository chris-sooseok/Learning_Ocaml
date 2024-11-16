



let empty = fun y -> None
let lookup key env = env key;;

(* will return None *)
lookup "foo" empty

let add key value env = fun y -> if key = y then Some value else env y;;