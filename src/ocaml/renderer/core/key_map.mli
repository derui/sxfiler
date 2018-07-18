
type module_type = Types.Viewer_module.t

type t

(** Return an empty handler mapping *)
val empty : t

(** Add a new bindings with key and handler function. *)
val add : t -> module_type:module_type -> bindings:Key_bindings.t -> t

(** Remove a handler mapped with given key. *)
val remove : t -> module_type:module_type -> t

(** Find action that is mapped key. Return None if no any action is available
    given key from argument.
*)
val find : t -> module_type:module_type -> Key_bindings.t option

(** [dump t] gets current key-value in [t]. *)
val dump: t -> (module_type * Key_bindings.t) list

(** [to_json t] returns json representation of [t]. *)
val to_json : t -> < > Js.t

(** [of_json js] returns new {!t} converted from [js] *)
val of_json : < > Js.t -> t
