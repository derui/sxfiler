
(** The type of key handler function *)
type action = Action.t

(** The module to map key and handler function.  *)
module Key_map : Map.S with type key = Sxfiler_kbd.t

type key_map = action Key_map.t

(** Return an empty handler mapping *)
val empty : key_map

(** Add a new mapping with key and handler function. *)
val add_key_map : key_map:key_map -> key:Key_map.key -> action:action -> key_map

(** Remove a handler mapped with given key. *)
val remove_key_map : key_map:key_map -> key:Key_map.key -> key_map

(** Dispatch key to handler mappings. Return None if no any handlers are available
    given key in argument.
*)
val dispatch : key_map:key_map -> key:Key_map.key -> Sxfiler_common.Message.t option
