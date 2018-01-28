module E = Sxfiler_common.Event
module P = Sxfiler_main_process

let keyboard_event_to_key v =
  let module K = Sxfiler_kbd in
  {K.key = Js.to_string v##.key;
   shift = Js.to_bool v##.shiftKey;
   meta = Js.to_bool v##.altKey;
   ctrl = Js.to_bool v##.ctrlKey;
  }

let on_key_event t ev v =
  let module K = Sxfiler_key_map in
  let module R = Sxfiler_flux_runner in
  let key = keyboard_event_to_key v in
  match K.dispatch ~key_map:t.P.key_map ~key with
  | None -> ()
  | Some message -> R.send t.P.runner message

let bind t =
  let key_listener ev = function
    | `Request_key_handling v -> on_key_event t ev v
    | _ -> ()
  in
  E.IPC.(on ~target:request_key_handling ~f:key_listener t.P.ipc)
