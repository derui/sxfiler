(** This use case do delete a notification.  *)

module T = Sxfiler_rpc.Types
module C = Sxfiler_renderer_core
module S = Sxfiler_renderer_service

type param = string
type t = {param : param}

let create param = {param}

let execute t dispatcher =
  let id = t.param in
  let module D = (val dispatcher : C.Dispatcher_intf.Instance) in
  Lwt.return D.(Dispatcher.dispatch this C.Message.(Delete_notification id))
