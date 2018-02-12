module C = Sxfiler_common

type key_action = C.Key_bindable_action.t
type message = C.Message.t

let to_message state = function
  | C.Key_bindable_action.Next_item -> C.Message.select_next_item 1
  | Prev_item -> C.Message.select_prev_item 1
  | Leave_directory -> C.Message.leave_directory
  | Enter_directory -> C.Message.enter_directory
  | Move_to_another -> C.Message.Move_to_another
  | Copy | Move | Delete -> failwith ""
  | Quit -> C.Message.request_quit_application
