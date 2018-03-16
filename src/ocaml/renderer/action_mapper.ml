module C = Sxfiler_common

type key_action = C.Key_bindable_action.t
type message = C.Message.t

let some v = Some v

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

let toggle_mark state =
  let module P = C.Types.Pane in
  let module F = C.Types.File_stat in
  let pane = C.State.active_pane state in
  let open Minimal_monadic_caml.Option.Infix in
  pane.P.focused_item >>= fun item -> some @@ C.Message.toggle_mark @@ F.to_js item

let to_message state = function
  | C.Key_bindable_action.Next_item -> some @@ select_item state `Next
  | Prev_item -> some @@ select_item state `Prev
  | Leave_directory -> some C.Message.leave_directory
  | Enter_directory -> some C.Message.enter_directory
  | Change_active_pane -> some C.Message.change_active_pane
  | Jump -> some @@ C.Message.open_dialog @@ C.Types.dialog_jump
  | Copy -> some @@ C.Message.open_dialog @@ C.Types.dialog_confirmation `Copy
  | Move -> some @@ C.Message.open_dialog @@ C.Types.dialog_confirmation `Move
  | Rename -> some @@ C.Message.open_dialog @@ C.Types.dialog_rename
  | Delete -> some @@ C.Message.open_dialog @@ C.Types.dialog_confirmation `Delete
  | Quit -> some @@ C.Message.quit_application
  | Toggle_mark -> toggle_mark state
