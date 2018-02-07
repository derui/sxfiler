(* This module provides types for message that is used as action on flux architecture *)
module T = Sxfiler_types

type t =
    Request_files_in_directory of (T.Pane_id.t * string)
  | Finish_files_in_directory of (T.Pane.t, exn) result
  | Request_quit_application
  | Add_pane of T.Pane.t
  | Select_next_item of int
  | Select_prev_item of int
  | Leave_directory
  | Enter_directory
[@@deriving variants]
