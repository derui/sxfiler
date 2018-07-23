(** This module provides only interface of collection. *)

(** Type of item of collection *)
module type Type = sig
  type t

  val to_string: t -> string
end
