module C = Sxfiler_common

let special_key_mapping = function
  | " " -> "Space"
  | _ as key -> key

let keyboard_event_to_key v =
  let module K = Sxfiler_kbd in
  Js.string @@ K.to_keyseq {
    K.key = special_key_mapping @@ Js.to_string v##.key;
    shift = Js.to_bool v##.shiftKey;
    meta = Js.to_bool v##.altKey;
    ctrl = Js.to_bool v##.ctrlKey;
  }

let get_focus_target = function
  | C.State.Dialog_state.Open _ -> Types.Focus_dialog
  | C.State.Dialog_state.Close -> Types.Focus_file_pane

let find_item_index ?(equal=(=)) ~v array =
  let rec find ary v ind =
    if Array.length ary <= ind then 0
    else if equal v ary.(ind) then ind
    else find ary v (succ ind)
  in

  find array v 0
