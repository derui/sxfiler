(* This module provides types for message that is used as action on flux architecture *)

type t =
    REQUEST_FILES_IN_DIRECTORY of string
  | FINISH_FILES_IN_DIRECTORY
[@@deriving variants]
