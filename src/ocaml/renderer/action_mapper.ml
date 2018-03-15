module C = Sxfiler_common

type key_action = C.Key_bindable_action.t
type message = C.Message.t

let select_item state direction =
  let module F = C.Types.File_stat in
  let module P = C.Types.Pane in
  let pane = C.State.active_pane state in
  let ind = match pane.P.focused_item with
    | None -> 0
    | Some item -> Util.find_item_index ~equal:F.equal ~v:item pane.P.file_list
  in

  let target_item = match direction with
    | `Next -> min (Array.length pane.P.file_list) (succ ind)
    | `Prev -> max 0 (pred ind)
  in

  C.Message.select_item @@ F.to_js pane.P.file_list.(target_item)

let to_message state = function
  | C.Key_bindable_action.Next_item -> select_item state `Next
  | Prev_item -> select_item state `Prev
  | Leave_directory -> C.Message.leave_directory
  | Enter_directory -> C.Message.enter_directory
  | Change_active_pane -> C.Message.change_active_pane
  | Jump -> C.Message.open_dialog @@ C.Types.dialog_jump
  | Copy -> C.Message.open_dialog @@ C.Types.dialog_confirmation `Copy
  | Move -> C.Message.open_dialog @@ C.Types.dialog_confirmation `Move
  | Rename -> C.Message.open_dialog @@ C.Types.dialog_rename
  | Delete -> C.Message.open_dialog @@ C.Types.dialog_confirmation `Delete
  | Quit -> C.Message.quit_application
