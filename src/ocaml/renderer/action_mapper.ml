module C = Sxfiler_common

type key_action = C.Key_bindable_action.t
type message = C.Message.t

let current_pane state =
  let current_id = state.C.State.current_pane in
  let module P = C.Types.Pane in
  let module PI = C.Types.Pane_id in
  List.find (fun pane -> PI.equal current_id pane.P.id) @@ Array.to_list state.C.State.panes

let pointed_file pane =
  let module P = C.Types.Pane in
  let pos = pane.P.cursor_pos
  and files = Array.of_list pane.P.file_list in
  files.(pos)

let target_pane_directory state =
  let current_id = state.C.State.current_pane in
  let module P = C.Types.Pane in
  let module PI = C.Types.Pane_id in
  let pane = List.find_opt (fun pane -> not @@ PI.equal current_id pane.P.id) @@ Array.to_list state.C.State.panes in
  match pane with
  | None -> Js.string ""
  | Some pane -> Js.string pane.P.directory

let to_message state = function
  | C.Key_bindable_action.Next_item -> C.Message.select_next_item 1
  | Prev_item -> C.Message.select_prev_item 1
  | Leave_directory -> C.Message.leave_directory
  | Enter_directory -> C.Message.enter_directory
  | Move_to_another -> C.Message.Move_to_another
  | Copy -> let pane = current_pane state in
    let file = pointed_file pane |> C.Types.File_stat.to_js in
    let module P = C.Message_payload in

    C.Message.request_copy_file P.Request_copy_file.({
        src = file;
        dest_dir = target_pane_directory state;
        same_name_behavior = Overwrite
      })
  | Move | Delete -> failwith ""
  | Quit -> C.Message.request_quit_application
