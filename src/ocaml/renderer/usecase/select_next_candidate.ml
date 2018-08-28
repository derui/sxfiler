(** This behavior selects next candidate in current completion session. *)
module C = Sxfiler_renderer_core

type param = unit
type t = unit

let create () = ()

let execute () dispatcher =
  let module D = (val dispatcher : C.Dispatcher_intf.Instance) in
  let message = C.Message.(Completion Select_next) in
  Lwt.return @@ D.(Dispatcher.dispatch this message)
