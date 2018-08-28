(* support functions to create class name for component *)

type class_name = string
type condition = class_name * bool
type t = condition list

val to_string : t -> string
