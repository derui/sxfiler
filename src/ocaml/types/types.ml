(** This module defines common and simple algebric types. *)

type cursor_pos = int

(** {!Sort_type} defines methods to sort in this software. *)
module Sort_type = struct
  type t =
    | Name
    | Size
    | Date
  [@@deriving enum,show]
end

type file_id = string

(** {!Layout} defines type to preset of layout *)
module Layout = struct
  type t = Side_by_side
  [@@deriving enum,show]
end

(** {!Source_type} defines type of source for completion.  *)
module Source_type = struct
  type t =
    | File
    | Directory_tree
    | History
  [@@deriving enum,show]
end

(** {!Candidate} defines type of result of completion. *)
module Candidate = struct
  type 'a t = {
    start: int ;
    length: int;
    value: 'a;
  }
end
