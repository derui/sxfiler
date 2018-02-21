module C = Sxfiler_common

let keyboard_event_to_key v =
  let module K = Sxfiler_kbd in
  Js.string @@ K.to_keyseq {
    K.key = Js.to_string v##.key;
    shift = Js.to_bool v##.shiftKey;
    meta = Js.to_bool v##.altKey;
    ctrl = Js.to_bool v##.ctrlKey;
  }

let get_focus_target = function
  | C.State.Dialog_state.Open _ -> Types.Focus_dialog
  | C.State.Dialog_state.Close -> Types.Focus_file_pane
