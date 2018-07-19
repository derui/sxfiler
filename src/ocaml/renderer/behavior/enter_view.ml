(** This module defines behavior to manage viewer context in store. *)
module T = Sxfiler_types_jsoo
module C = Sxfiler_renderer_core

type t = unit

type param = (module C.Store_intf.Instance with type message = C.Message.t) * C.Types.Viewer_module.t
type result = unit

let make _ = ()

let execute () (store, module_) =
  let module Store = (val store : C.Store_intf.Instance with type message = C.Message.t) in
  match module_ with
  | C.Types.Viewer_module.File_tree -> Store.(Store.dispatch instance C.Message.Enter_file_tree_context)
  | _ -> ()
