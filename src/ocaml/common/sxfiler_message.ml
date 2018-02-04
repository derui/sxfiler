(* This module provides types for message that is used as action on flux architecture *)
module T = Sxfiler_types

type t =
    REQUEST_FILES_IN_DIRECTORY of string
  | FINISH_FILES_IN_DIRECTORY of (exn option * string * T.File_stat.t array)
  | REQUEST_QUIT_APPLICATION
  | SELECT_NEXT_ITEM of int
  | SELECT_PREV_ITEM of int
  | LEAVE_DIRECTORY
  | ENTER_DIRECTORY
[@@deriving variants]
