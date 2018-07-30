(** This module defines behavior to set next action to execute. *)
module C = Sxfiler_renderer_core

type param = C.Callable_action.t
type t = {
  action : param;
}

type config = unit

let create () param = {action = param}

let execute t dispatcher =
  let module D = (val dispatcher : C.Dispatcher_intf.Instance) in
  Lwt.return @@ D.(Dispatcher.dispatch this @@ C.Message.(Command (Select t.action)))
