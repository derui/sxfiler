(** This module provides types for message that is used as action on flux architecture *)
module T = Common_types

module P = Common_message_payload

(** The type of message. This allows to pass to Javascript native functions all variant.

    We define message naming convention:

    - All message must have verb as first.
    - If action bound message is asynchronous, add {_request} for request, and {_response} for result
*)
type t =
    Update_pane_request of (T.Pane.js Js.t * Js.js_string Js.t * T.Pane_location.js Js.t)
  | Update_pane_response of (T.Pane.js Js.t * T.Pane_location.js Js.t, T.Operation_log.Entry.js Js.t) result
  | Refresh_panes_request
  | Refresh_panes_response of (T.Pane.js Js.t * T.Pane.js Js.t, T.Operation_log.Entry.js Js.t) result
  | Quit_application
  | Select_next_item of int
  | Select_prev_item of int
  | Leave_directory
  | Enter_directory
  | Change_active_pane
  | Open_dialog of T.dialog_type
  | Close_dialog of T.User_action.js Js.t
  | Execute_task_request of T.Task_request.js Js.t
  | Execute_task_response of T.Task_result.js Js.t
[@@deriving variants]
