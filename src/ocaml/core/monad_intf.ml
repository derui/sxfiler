(** module type for type to want to make monadic interface *)
module type Base = sig
  type 'a t

  val bind : 'a t -> f:('a -> 'b t) -> 'b t

  val fmap : [ `Use_bind_to_define | `Use_original       of 'a t -> f:('a -> 'b) -> 'b t ]

  val return : 'a -> 'a t
end

(** module interface of monad. This interface apply to single placeholder only, but you can use this interface with type
    having placeholder more than two if you specify other placeholders. *)
module type S = sig
  type 'a t

  val bind : 'a t -> f:('a -> 'b t) -> 'b t
  (** bind each monad *)

  val fmap : 'a t -> f:('a -> 'b) -> 'b t
  (** apply function to value of monad *)

  val return : 'a -> 'a t
  (** wrap value with monad *)

  val lift : ('a -> 'b) -> 'a -> 'b t

  module Infix : sig
    val ( >>= ) : 'a t -> ('a -> 'b t) -> 'b t

    val ( >>| ) : 'a t -> ('a -> 'b) -> 'b t

    val ( let* ) : 'a t -> ('a -> 'b t) -> 'b t

    val ( let+ ) : 'a t -> ('a -> 'b) -> 'b t
  end
end

(** module type for type to want to make monadic interface.

    This interface will manage first placeholder only, second placeholder do not manage in this. *)
module type Base2 = sig
  type ('a, 'b) t

  val bind : ('a, 'b) t -> f:('a -> ('c, 'b) t) -> ('c, 'b) t

  val fmap : [ `Use_bind_to_define | `Use_original       of ('a, 'b) t -> f:('a -> 'c) -> ('c, 'b) t ]

  val return : 'a -> ('a, 'b) t
end

(** module type of monad signature having two placeholder *)
module type S2 = sig
  type ('a, 'b) t

  val bind : ('a, 'b) t -> f:('a -> ('c, 'b) t) -> ('c, 'b) t
  (** bind each monad *)

  val fmap : ('a, 'b) t -> f:('a -> 'c) -> ('c, 'b) t
  (** apply function to value of monad *)

  val return : 'a -> ('a, 'b) t
  (** wrap value with monad *)

  val lift : ('a -> 'c) -> 'a -> ('c, 'b) t
  (** Lift function to monadic *)

  module Infix : sig
    val ( >>= ) : ('a, 'b) t -> ('a -> ('c, 'b) t) -> ('c, 'b) t

    val ( >>| ) : ('a, 'b) t -> ('a -> 'c) -> ('c, 'b) t

    val ( let* ) : ('a, 'b) t -> ('a -> ('c, 'b) t) -> ('c, 'b) t

    val ( let+ ) : ('a, 'b) t -> ('a -> 'c) -> ('c, 'b) t
  end
end
