module C = Sxfiler_common

type key_action = C.Key_bindable_action.t
type message = C.Message.t

let to_message state = function
  | C.Key_bindable_action.Next_item -> C.Message.select_next_item 1
  | Prev_item -> C.Message.select_prev_item 1
  | Leave_directory -> C.Message.leave_directory
  | Enter_directory -> C.Message.enter_directory
  | Move_to_another -> C.Message.Move_to_another
  | Copy -> let pane = C.State.active_pane state in
    let inactive_pane = C.State.inactive_pane state in
    let file = C.State.Pane.pointed_file pane |> C.Types.File_stat.to_js in
    let module P = C.Message_payload in
    let module Pane = C.Types.Pane in

    C.Message.request_operation @@ C.Message.Operation.Copy P.Request_copy_file.({
        src = file;
        dest_dir = Js.string inactive_pane.Pane.directory;
        same_name_behavior = Overwrite
      })
  | Move | Delete -> failwith ""
  | Quit -> C.Message.request_quit_application
