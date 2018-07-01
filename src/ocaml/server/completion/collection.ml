(** This module provides only interface of colleciton. *)

(** Type of item of collection *)
module type Type = sig
  type t

  val to_string: t -> string
end
