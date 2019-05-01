type t [@@deriving show, eq]

val make : unit -> t
(** [make ()] Return an empty keymap with identifier [id]. *)

val add : t -> condition:Condition.t -> key:Sxfiler_kbd.t -> value:string -> t
(** Add a new bindings with key and handler function. *)

val find : t -> condition:Condition.t -> key:Sxfiler_kbd.t -> string option
(** Find action that is mapped key. Return None if no any action is available
    given key from argument.
*)

val subset : t -> condition:Condition.t -> t
(** [subset keymap ~condition] gets the subset of [keymap] that are matched with clause
    [condition].
*)

val bindings : t -> (Condition.t * Sxfiler_kbd.t * string) list
(** [bindings t] returns current bindings with condition. *)
