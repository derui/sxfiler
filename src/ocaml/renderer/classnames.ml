(* support functions to create class name for component *)
type class_name = string
type condition = string * bool
type t = condition list

let to_string t =
  String.concat " " @@ List.rev @@ List.map fst @@ List.filter snd t
