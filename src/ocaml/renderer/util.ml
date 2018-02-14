
let keyboard_event_to_key v =
  let module K = Sxfiler_kbd in
  Js.string @@ K.to_keyseq {
    K.key = Js.to_string v##.key;
    shift = Js.to_bool v##.shiftKey;
    meta = Js.to_bool v##.altKey;
    ctrl = Js.to_bool v##.ctrlKey;
  }
