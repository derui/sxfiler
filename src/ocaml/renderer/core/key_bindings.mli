(** the type of action *)
type action = Callable_action.t

(** the type of key *)
type key = string

(** the type of t *)
type t

(** [empty] gets empty key bindings *)
val empty: t

(** [add t ~key ~action] return new [t] that is added new binging [key] and [action]. *)
val add : t -> key:key -> action:action -> t

(** [remove t ~key] return new [t] that is removed binding specified by [key]. *)
val remove : t -> key:key -> t

(** [find t ~key] return key bindings if it exists. *)
val find : t -> key:key -> action option

(** [dump t] returns all bindings in [t] as list. *)
val dump : t -> (key * action) list
