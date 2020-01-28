module Identity : sig
  module type S = sig
    type t [@@deriving eq, show, ord]

    type value

    val make : value -> t
    (** [make id] makes the id value of [id] *)

    val value : t -> value
    (** [value t] unwrap value from [t] *)
  end

  module type Value = sig
    type t [@@deriving eq, show, ord]
  end

  module Make (V : Value) : S with type value := V.t
end

type 'a command = { data : 'a }

(** Common type for non empty string *)
module Not_empty_string : sig
  type t [@@deriving eq, show, ord]

  val make : string -> t option
  (** make a not empty string from a string. If string is empty, then return None from this *)

  val value : t -> string
  (** return the value of [t] *)
end

module Positive_number : sig
  type t [@@deriving eq, show, ord]

  val make : int -> t option
  (** make a positive number from integer. Return None if given value is less or equal 0 *)

  val value : t -> int
  (** unwrap value of [t] *)
end
