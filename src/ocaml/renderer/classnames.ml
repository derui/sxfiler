(* support functions to create class name for component *)
type class_name = string
type condition = string * bool
type t = string list

let empty = []
let return v = [v]

let to_string t = String.concat " " @@ List.rev t

let join list (cls, condition) = if condition then cls :: list else list

module Infix = struct
  let (<|>) = join
end
