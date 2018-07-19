(** This module defines behavior to manage viewer context in store. *)
module T = Sxfiler_types_jsoo
module C = Sxfiler_renderer_core

type message = C.Message.t
type t = unit

type param = C.Types.Viewer_module.t
type result = unit

let make _ = ()

let execute () dispatcher module_ =
  let module D = (val dispatcher: C.Dispatcher_intf.Instance with type message = C.Message.t) in
  match module_ with
  | C.Types.Viewer_module.File_tree -> D.(Dispatcher.dispatch instance C.Message.Leave_file_tree_context)
  | _ -> ()
