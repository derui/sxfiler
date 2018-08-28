module type S = sig
  (** the type of message will be sent from behavior. *)
  type message

  (** The type of state *)
  type t

  val reduce : t -> message -> t
  (** [update t event] will update state via event [message]. state can not update directly.  *)

  val equal : t -> t -> bool
  (** [equal t1 t2] return equivlant between [t1] and [t2] *)
end
