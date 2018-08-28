(** The signature of key map *)
module type S = sig
  type 'a t

  val make : unit -> 'a t
  (** [make id] Return an empty keymap with identifier [id]. *)

  val add : 'a t -> condition:Condition.t -> key:Sxfiler_kbd.t -> value:'a -> 'a t
  (** Add a new bindings with key and handler function. *)

  val find : 'a t -> condition:Condition.t -> key:Sxfiler_kbd.t -> 'a option
  (** Find action that is mapped key. Return None if no any action is available
      given key from argument.
  *)

  val subset : 'a t -> condition:Condition.t -> 'a t
  (** [subset keymap ~condition] gets the subset of [keymap] that are matched with clause
      [condition].
  *)

  val bindings : 'a t -> (Condition.t * Sxfiler_kbd.t * 'a) list
  (** [bindings t] returns current bindings with condition. *)
end
