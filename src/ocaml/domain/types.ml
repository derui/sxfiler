(** This module defines common and simple algebric types. *)

type cursor_pos = int

(** {!Sort_type} defines methods to sort in this software. *)
module Sort_type = struct
  type t =
    | Name
    | Size
    | Date
end

type file_id = string

(** {!Layout} defines type to preset of layout *)
module Layout = struct
  type t = Side_by_side
end

(** Base signature of thread. *)
module type Thread = sig
  type 'a t

  val bind: 'a t -> ('a -> 'b t) -> 'b t
  val return: 'a -> 'a t
end