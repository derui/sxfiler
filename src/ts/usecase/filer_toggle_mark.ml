(** This module defines behavior to toggle mark of current selected item. *)
module C = Sxfiler_renderer_core

type param = unit
type t = unit

let create () = ()

let execute () dispatcher =
  let module D = (val dispatcher : C.Dispatcher_intf.Instance) in
  D.(Dispatcher.dispatch this @@ C.Message.Toggle_mark) ;
  D.(Dispatcher.dispatch this C.Message.Move_cursor_to_next) |> Lwt.return
