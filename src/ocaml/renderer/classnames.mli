(* support functions to create class name for component *)
type class_name = string
type condition = class_name * bool
type t

val empty : t
val return : class_name -> t
val join : t -> condition -> t

val to_string : t -> string

module Infix : sig
  val (<|>) : t -> condition -> t
end
