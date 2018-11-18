(** This module defines behavior to move cursor of current filer. *)
module C = Sxfiler_renderer_core

type param =
  [ `Next
  | `Prev ]

type t = {direction : param}

let create param = {direction = param}

let execute t dispatcher =
  let module D = (val dispatcher : C.Dispatcher_intf.Instance) in
  match t.direction with
  | `Next -> Lwt.return @@ D.(Dispatcher.dispatch this @@ C.Message.Move_cursor_to_next)
  | `Prev -> Lwt.return @@ D.(Dispatcher.dispatch this @@ C.Message.Move_cursor_to_prev)
