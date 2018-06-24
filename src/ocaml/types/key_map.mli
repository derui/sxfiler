
(** The type of key handler function *)
type action = Callable_action.t

type key = string

type t

(** Return an empty handler mapping *)
val empty : t

(** Add a new mapping with key and handler function. *)
val add : t -> key:key -> action:action -> t

(** Remove a handler mapped with given key. *)
val remove : t -> key:key -> t

(** Find action that is mapped key. Return None if no any action is available
    given key from argument.
*)
val find : t -> key:key -> action option

(** [dump t] returns list of key-action added by [add] before. *)
val dump : t -> (key * action) list
