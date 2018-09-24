module type S = sig
  type 'a t

  val wrap : ('a -> 'b) -> 'a t * ('a -> 'b)
  (** [wrap f] returns wrapped function and spy instance. *)

  val called_args : 'a t -> 'a list
  (** [called_args t] returns arguments before called spied function. *)

  val called_count : 'a t -> int
  (** [called_count t] returns number of called count spied function.   *)
end

module type S2 = sig
  type ('a, 'b) t

  val wrap : ('a -> 'b -> 'c) -> ('a, 'b) t * ('a -> 'b -> 'c)
  (** [wrap f] returns wrapped function and spy instance. *)

  val called_args : ('a, 'b) t -> ('a * 'b) list
  (** [called_args t] returns arguments before called spied function. *)

  val called_count : ('a, 'b) t -> int
  (** [called_count t] returns number of called count spied function.   *)
end
