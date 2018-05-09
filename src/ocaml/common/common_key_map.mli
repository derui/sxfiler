
(** The type of key handler function *)
type action = Callable_action.t

(** The module to map key and handler function.  *)
module Key_map : Map.S with type key = string

type t = action Key_map.t

(** Type of js for key map. *)
type js = < >

(** Return an empty handler mapping *)
val empty : t

(** Add a new mapping with key and handler function. *)
val add_key_map : key_map:t -> key:Key_map.key -> action:action -> t

(** Remove a handler mapped with given key. *)
val remove_key_map : key_map:t -> key:Key_map.key -> t

(** Find action that is mapped key. Return None if no any action is available
    given key from argument.
*)
val find : key_map:t -> key:Key_map.key -> action option

(** Convert JS object to instance of key map type *)
val of_js : js Js.t -> t
