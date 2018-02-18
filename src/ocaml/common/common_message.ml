(** This module provides types for message that is used as action on flux architecture *)
module T = Common_types

module P = Common_message_payload

(** The type of message. This allows to pass to Javascript native functions all variant. *)
type t =
    Request_files_in_directory of (T.Pane_id.t * string)
  | Finish_files_in_directory of (T.Pane.js Js.t, exn) result
  | Request_quit_application
  | Select_next_item of int
  | Select_prev_item of int
  | Leave_directory
  | Enter_directory
  | Move_to_another
  | Request_copy_file of P.Request_copy_file.t
  | Request_confirm_operation
  | Request_cancel_operation
[@@deriving variants]
