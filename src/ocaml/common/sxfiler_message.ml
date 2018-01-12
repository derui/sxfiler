(* This module provides types for message that is used as action on flux architecture *)
module T = Sxfiler_types

type t =
    REQUEST_FILES_IN_DIRECTORY of string
  | FINISH_FILES_IN_DIRECTORY of (exn option * string * T.File_stat.t array)
  | REQUEST_QUIT_APPLICATION
[@@deriving variants]
