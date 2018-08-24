(** This module defines behavior to swap current filer. *)
module C = Sxfiler_renderer_core

type param = unit
type t = unit

let create () = ()

let execute () dispatcher =
  let module D = (val dispatcher : C.Dispatcher_intf.Instance) in
  Lwt.return @@ D.(Dispatcher.dispatch this @@ C.Message.Swap_filer)
