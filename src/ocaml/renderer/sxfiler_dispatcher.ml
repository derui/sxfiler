module C = Sxfiler_common.Std.Message
module E = Sxfiler_common.Std.Event

type t = (C.t -> unit) Js.callback

let make ipc =
  Js.wrap_callback @@ (fun message -> E.IPC.send ~channel:(`Action message) ~ipc)

let dispatch dispatcher message =
  Js.Unsafe.fun_call dispatcher [|Js.Unsafe.inject message|]
