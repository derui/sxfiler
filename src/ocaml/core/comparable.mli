(** The module to provide type and compare function as functor *)
module type Type = sig
  type t

  val compare : t -> t -> int
end

(** {!S} is signature for Comparable module. *)
module type S = sig
  type t

  val compare : t -> t -> int

  (** {!Infix} defines infix operators *)
  module Infix : sig
    val ( = ) : t -> t -> bool

    val ( < ) : t -> t -> bool

    val ( > ) : t -> t -> bool

    val ( <= ) : t -> t -> bool

    val ( >= ) : t -> t -> bool

    val ( <> ) : t -> t -> bool
  end
end

(** Functor to create comparable module. Usecase of this module is including in a module *)
module Make (T : Type) : S with type t := T.t
