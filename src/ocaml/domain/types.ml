(** This module defines common and simple algebric types. *)

type cursor_pos = int

(** {!Sort_type} defines methods to sort in this software. *)
module Sort_type = struct
  type t =
    | Name
    | Size
    | Date
  [@@deriving show]

  (** [to_int t] returns int representation of [t] *)
  let to_int = function Name -> 1 | Size -> 2 | Date -> 3

  (** [of_int int] returns [t] related given argument. *)
  let of_int = function 1 -> Some Name | 2 -> Some Size | 3 -> Some Date | _ -> None
end

type file_id = string

(** Base signature of thread. *)
module type Thread = Sxfiler_core.Monad.S

(** {!type:correction} takes method to avoid error in transportation *)
module Correction = struct
  type method_ = Name of string [@@deriving show]

  type t =
    { node_id : string
    ; method_ : method_ }
  [@@deriving show]

  let name id name = {node_id = id; method_ = Name name}
end

type corrections = Correction.t list [@@deriving show]
