
module type Type = sig
  type t

  val to_string: t -> string
end
