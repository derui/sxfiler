(** Type for current renderer context. User should use this if call RPC, lookup state, or update state. *)
module C = Sxfiler_renderer_core

type t = {
  mutable store: C.Store_group.t;
  rpc: (module Rpc.Rpc);
  locator: (module C.Locator_intf.S);
}

let make rpc locator =
  let module C = Sxfiler_renderer_core in
  let group = C.Store_group.create () in
  let group = C.Store_group.set group ~tag:Store.config ~v:(Store.Config_store.make ()) in
  let group = C.Store_group.set group ~tag:Store.viewer_stacks ~v:(Store.Viewer_stacks_store.make ()) in
  let group = C.Store_group.set group ~tag:Store.layout ~v:(Store.Layout_store.make ()) in
  {
    store = group;
    rpc;
    locator;
  }

(** [get_store t ~tag] is helper function to get store directly.  *)
let get_store t ~tag = C.Store_group.get t.store ~tag

let subscribe t event = t.store <- C.Store_group.subscribe t.store ~event

(** [execute instance param] execute behavior [instance] with [param]. *)
let execute
    (type p)
    (type r)
    t
    behavior
    (param:p) =
  let module Be = (val behavior : C.Behavior_intf.S with type param = p and type result = r) in
  let behavior = Be.make t.locator in
  Be.execute behavior param
