module C = Sxfiler_common

type key_action = C.Key_bindable_action.t
type message = C.Message.t

let to_message state = function
  | C.Key_bindable_action.Next_item -> C.Message.select_next_item 1
  | Prev_item -> C.Message.select_prev_item 1
  | Leave_directory -> C.Message.leave_directory
  | Enter_directory -> C.Message.enter_directory
  | Change_active_pane -> C.Message.change_active_pane
  | Copy -> C.Message.open_dialog @@ C.Types.dialog_confirmation `Copy
  | Move -> C.Message.open_dialog @@ C.Types.dialog_confirmation `Move
  | Rename -> C.Message.open_dialog @@ C.Types.dialog_rename
  | Delete -> C.Message.open_dialog @@ C.Types.dialog_confirmation `Delete
  | Quit -> C.Message.quit_application
