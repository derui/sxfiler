(** This module defines behavior to move cursor of current scanner. *)
module C = Sxfiler_renderer_core

type param = [`Next | `Prev]
type t = {
  direction : param;
}

type config = unit

let create () param = {direction = param}

let execute t dispatcher =
  let module D = (val dispatcher : C.Dispatcher_intf.Instance) in
  match t.direction with
  | `Next -> `Unit D.(Dispatcher.dispatch this @@ C.Message.Move_cursor_to_next)
  | `Prev -> `Unit D.(Dispatcher.dispatch this @@ C.Message.Move_cursor_to_prev)
