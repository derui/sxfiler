(** This module provides types for message that is used as action on flux architecture *)
module T = Common_types

module P = Common_message_payload

module Operation = struct
  type t =
    | Copy of P.Request_copy_file.t
    | Delete of P.Request_delete_file.t
    | Move of P.Request_move_file.t
    | Rename of P.Request_move_file.t
end

(** The type of message. This allows to pass to Javascript native functions all variant.

    We define message naming convention:

    - All message must have verb as first.
    - If action bound message is asynchronous, add {_request} for request, and {_response} for result
*)
type t =
    Update_pane_request of (T.Pane.js Js.t * Js.js_string Js.t * T.Pane_location.js Js.t)
  | Update_pane_response of (T.Pane.js Js.t * T.Pane_location.js Js.t) T.Operation_result.t
  | Refresh_panes_request
  | Refresh_panes_response of (T.Pane.js Js.t * T.Pane.js Js.t) T.Operation_result.t
  | Quit_application
  | Select_next_item of int
  | Select_prev_item of int
  | Leave_directory
  | Enter_directory
  | Change_active_pane
  | Request_operation of Operation.t
  | Execute_operation_request of Operation.t
  | Execute_operation_response of unit T.Operation_result.t
  | Confirm_operation of bool
[@@deriving variants]
