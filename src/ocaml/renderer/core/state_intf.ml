module type S = sig
  (** the type of message will be sent from behavior. *)
  type message

  (** The type of state *)
  type t

  (** [empty ()] gets a new instance of [t]. *)
  val empty: unit -> t

  (** [update t event] will update state via event [message]. *)
  val update: t -> message -> t
end
