module E = Sxfiler_common.Event
module W = Window_binder
module Kbd = Sxfiler_kbd

let on_action t ev message =
  let module R = Flux_runner in
  R.send t.W.runner message

let bind t =
  let key_listener ev = function
    | E.IPC.Action v -> on_action t ev v
    | _ -> ()
  in
  E.IPC.(on ~target:Listener.action ~f:key_listener t.W.ipc)
