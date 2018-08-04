
(** The signature of key map *)
module type S = sig
  type 'a t

  (** [id t] get identifier of keymap [t]  *)
  val id: 'a t -> string

  (** [make id] Return an empty keymap with identifier [id]. *)
  val make : string -> 'a t

  (** Add a new bindings with key and handler function. *)
  val add : 'a t -> condition:Condition.t -> key:Sxfiler_kbd.t -> value:'a -> 'a t

  (** Find action that is mapped key. Return None if no any action is available
      given key from argument.
  *)
  val find : 'a t -> condition:Condition.t -> key:Sxfiler_kbd.t -> 'a option

  (** [bindings t] returns current bindings with condition. *)
  val bindings: 'a t -> (Condition.t * Sxfiler_kbd.t * 'a) list
end

(** Interface of repository for key map. *)
module type Repository = sig

  module Key_map : S
  module Thread: Types.Thread

  (** [save keymap] save [keymap] to store anywhere. *)
  val save: 'a Key_map.t -> unit Thread.t

  (** [resolve ] get *)
  val resolve: string -> 'a Key_map.t Thread.t
end
