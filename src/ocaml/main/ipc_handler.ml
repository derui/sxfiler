module E = Sxfiler_common.Event
module P = Main_process
module Kbd = Sxfiler_kbd

let on_key_event t ev (key, event_type) =
  let module K = Key_map in
  let module R = Flux_runner in
  let key = Kbd.of_js key in

  let module KE = Reactjscaml.Event.Keyboard_event in

  match event_type with
  | KE.KeyUp | KE.KeyPress -> ()
  | _ -> begin
      match K.dispatch ~key_map:t.P.key_map ~key with
      | None -> ()
      | Some message -> R.send t.P.runner message
    end

let bind t =
  let key_listener ev = function
    | E.IPC.Request_key_handling v -> on_key_event t ev v
    | _ -> ()
  in
  E.IPC.(on ~target:Listener.request_key_handling ~f:key_listener t.P.ipc)
