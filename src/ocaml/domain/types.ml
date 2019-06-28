(** This module defines common and simple algebric types. *)

type cursor_pos = int

(** {!Sort_type} defines methods to sort in this software. *)
module Sort_type = struct
  type t =
    | Name
    | Size
    | Date
  [@@deriving eq, show]
end

type file_id = string

module type Thread = Sxfiler_core.Monad.S
(** Base signature of thread. *)
