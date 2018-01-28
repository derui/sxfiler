module E = Sxfiler_common.Event
module P = Sxfiler_main_process

let keyboard_event_to_key v =
  let module K = Sxfiler_kbd in
  {K.key = Js.to_string v##.key;
   shift = Js.to_bool v##.shiftKey;
   meta = Js.to_bool v##.altKey;
   ctrl = Js.to_bool v##.ctrlKey;
  }

let on_key_event t sender ev v =
  let module K = Sxfiler_key_handler in
  let key = keyboard_event_to_key v in
  match K.dispatch ~handlers:t.P.key_handler_map ~key with
  | None -> ()
  | Some act -> sender act

let bind ~action_handler t =
  let key_listener ev = function
    | `Request_key_handling v -> on_key_event t action_handler ev v
    | _ -> ()
  in
  E.IPC.(on ~target:request_key_handling ~f:key_listener t.P.ipc)
