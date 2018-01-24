module C = Sxfiler_common.Message
module E = Sxfiler_common.Event

type t = (C.t -> unit) Js.callback

let make ipc =
  Js.wrap_callback @@ (fun message -> E.IPC.send ~channel:(`Action (C.to_js message)) ~ipc)

let dispatch : t -> C.t -> unit = fun dispatcher message ->
  Js.Unsafe.fun_call dispatcher [|Js.Unsafe.inject message|]
