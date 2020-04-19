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

module Make (T : Type) : S with type t := T.t = struct
  let compare = T.compare

  (** {!Infix} defines infix operators *)
  module Infix = struct
    let ( = ) v1 v2 = T.compare v1 v2 |> Int.equal 0

    let ( < ) v1 v2 = T.compare v1 v2 |> Int.equal (-1)

    let ( > ) v1 v2 = T.compare v1 v2 |> Int.equal 1

    let ( <= ) v1 v2 = not (v1 > v2)

    let ( >= ) v1 v2 = not (v1 < v2)

    let ( <> ) v1 v2 = not (v1 = v2)
  end
end
