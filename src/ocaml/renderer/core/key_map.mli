
type t

(** Return an empty handler mapping *)
val empty : t

(** Add a new bindings with key and handler function. *)
val add : t -> condition:Types.Condition.t -> key:string -> action:Callable_action.t -> t

(** Find action that is mapped key. Return None if no any action is available
    given key from argument.
*)
val find : t -> condition:Types.Condition.t -> Key_bindings.t

(** [to_json t] returns json representation of [t]. *)
val to_json : t -> < > Js.t

(** [of_json js] returns new {!t} converted from [js] *)
val of_json : < > Js.t -> t
