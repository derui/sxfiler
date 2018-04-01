module C = Sxfiler_common

type key_action = C.Key_bindable_action.t
type message = C.Message.t

let some v = Some v

let toggle_mark state =
  let module P = C.Types.Pane in
  let module F = C.Types.File_id in
  let pane = C.State.active_pane state in
  let open Minimal_monadic_caml.Option in
  let open Infix in
  pane.P.focused_item >>= lift @@ fun (id, _) -> C.Message.toggle_mark @@ F.to_js id

let to_message state = function
  | C.Key_bindable_action.Next_item -> some @@ C.Message.select_next_item
  | Prev_item -> some @@ C.Message.select_prev_item
  | Leave_directory -> some C.Message.leave_directory
  | Enter_directory -> some C.Message.enter_directory
  | Change_active_pane -> some C.Message.change_active_pane
  | Jump -> some @@ C.Message.open_dialog @@ C.Types.dialog_jump
  | Copy -> some @@ C.Message.open_dialog @@ C.Types.dialog_confirmation `Copy
  | Move -> some @@ C.Message.open_dialog @@ C.Types.dialog_confirmation `Move
  | Rename -> some @@ C.Message.open_dialog @@ C.Types.dialog_name_input `Rename
  | Delete -> some @@ C.Message.open_dialog @@ C.Types.dialog_confirmation `Delete
  | Quit -> some @@ C.Message.quit_application
  | Toggle_mark -> toggle_mark state
  | Make_dir -> some @@ C.Message.open_dialog @@ C.Types.dialog_name_input `Mkdir
  | History -> some @@ C.Message.open_dialog @@ C.Types.dialog_history
  | Change_permission -> some @@ C.Message.open_dialog @@ C.Types.dialog_change_permission
