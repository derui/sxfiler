(** This module defines behavior for initialize stores. *)
module T = Sxfiler_types_jsoo
module C = Sxfiler_renderer_core

type t = {
  scanner_repo: (module C.Repository_intf.Scanner_instance);
}

type param = (module C.Store_intf.Instance with type message = C.Message.t)
type result = unit

let make locator =
  let module L = (val locator : C.Locator_intf.S) in
  {
    scanner_repo = (module L.Repository.Scanner)
  }

let execute t store =
  let module R = (val t.scanner_repo) in let open R in
  let module Store = (val store : C.Store_intf.Instance with type message = C.Message.t) in
  Repo.on_change instance @@ fun v ->
  Store.(Store.dispatch instance C.Message.(Update_scanner v))
