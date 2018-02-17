
(** The type of key handler function *)
type action = Common_key_bindable_action.t

(** The module to map key and handler function.  *)
module Key_map : Map.S with type key = Js.js_string Js.t

type t = action Key_map.t

(** Return an empty handler mapping *)
val empty : t

(** Add a new mapping with key and handler function. *)
val add_key_map : key_map:t -> key:Key_map.key -> action:action -> t

(** Remove a handler mapped with given key. *)
val remove_key_map : key_map:t -> key:Key_map.key -> t

(** Dispatch key to handler mappings. Return None if no any handlers are available
    given key in argument.
*)
val dispatch : key_map:t -> key:Key_map.key -> action option
