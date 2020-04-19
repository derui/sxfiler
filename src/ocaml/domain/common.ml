module Identity = struct
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

  module Make (V : Value) : S with type value := V.t = struct
    type t = Id of V.t [@@deriving eq, show, ord]

    let make v = Id v

    let value (Id v) = v
  end
end

type 'a command = { data : 'a }

module Not_empty_string = struct
  type t = T of string [@@deriving eq, show, ord]

  let make v = if String.length v <= 0 then None else Option.some (T v)

  let value (T v) = v
end

module Positive_number = struct
  type t = T of int [@@deriving eq, show, ord]

  let make v = if v <= 0 then None else T v |> Option.some

  let value (T v) = v
end
