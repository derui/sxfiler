(** This module defines common and simple algebric types. *)

type cursor_pos = int

(** {!Sort_type} defines methods to sort in this software. *)
module Sort_type = struct
  type t =
    | Name
    | Size
    | Date

  (** [to_int t] returns int representation of [t] *)
  let to_int = function
    | Name -> 1
    | Size -> 2
    | Date -> 3

  (** [of_int int] returns [t] related given argument. *)
  let of_int = function
    | 1 -> Some Name
    | 2 -> Some Size
    | 3 -> Some Date
    | _ -> None
end

type file_id = string

(** Base signature of thread. *)
module type Thread = Sxfiler_core.Monad.S
