(** type for binding context and key *)
module Binding : sig
  type t = private {
    context : Context.t;
    key : Sxfiler_kbd.t;
  }
  [@@deriving show, eq, ord]

  val make : context:Context.t -> key:Sxfiler_kbd.t -> t
  (** [make ~context ~key] makes new instance of [t] *)
end

module Action : sig
  type t [@@deriving show, eq]

  val make : string -> t

  val value : t -> string
end

type t [@@deriving show, eq]

val empty : t
(** [empty] Return an empty keymap. *)

val add : binding:Binding.t -> action:Action.t -> t -> t
(** Add a new bindings with key and handler function. *)

val remove : binding:Binding.t -> t -> t
(** Remove a bindings from [t]. *)

val find_by : key:Sxfiler_kbd.t -> t -> Binding.t list
(** Find all bindings bound by the [key] *)

val bindings : t -> (Binding.t * Action.t) list
(** [bindings t] returns current bindings with condition. *)
