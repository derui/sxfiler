type contexts = string list
type t [@@deriving show, eq]

val make : unit -> t
(** [make ()] Return an empty keymap with identifier [id]. *)

val add : t -> contexts:contexts -> key:Sxfiler_kbd.t -> value:string -> t
(** Add a new bindings with key and handler function. *)

val remove : t -> contexts:contexts -> key:Sxfiler_kbd.t -> t
(** Remove a bindings from [t] in the [contexts]. *)

val bindings : t -> (contexts * Sxfiler_kbd.t * string) list
(** [bindings t] returns current bindings with condition. *)
