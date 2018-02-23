(** This module provides types for message that is used as action on flux architecture *)
module T = Common_types

module P = Common_message_payload

module Operation = struct
  type t =
    | Copy of P.Request_copy_file.t
end

(** The type of message. This allows to pass to Javascript native functions all variant. *)
type t =
    Request_files_in_directory of (T.Pane.js Js.t * Js.js_string Js.t)
  | Finish_files_in_directory of T.Pane.js Js.t T.Operation_result.t
  | Request_refresh_panes
  | Finish_refresh_panes of (T.Pane.js Js.t * T.Pane.js Js.t) T.Operation_result.t
  | Request_quit_application
  | Select_next_item of int
  | Select_prev_item of int
  | Leave_directory
  | Enter_directory
  | Move_to_another
  | Request_operation of Operation.t
  | Request_execute_operation of Operation.t
  | Finish_execute_operation of unit T.Operation_result.t
  | Confirm_operation of bool
[@@deriving variants]
