(** Type for current renderer context. User should use this if call RPC, lookup state, or update state. *)
module C = Sxfiler_renderer_core

type t = {
  mutable store: C.Store_group.t;
  rpc: (module Rpc.Rpc);
}

let make rpc =
  let module C = Sxfiler_renderer_core in
  let group = C.Store_group.create () in
  let group = C.Store_group.set group ~tag:Store.config ~v:(Store.Config_store.make ()) in
  let group = C.Store_group.set group ~tag:Store.viewer_stacks ~v:(Store.Viewer_stacks_store.make ()) in
  let group = C.Store_group.set group ~tag:Store.layout ~v:(Store.Layout_store.make ()) in
  {
    store = group;
    rpc;
  }

(** [get_store t ~tag] is helper function to get store directly.  *)
let get_store t ~tag = C.Store_group.get t.store ~tag

let subscribe t event = t.store <- C.Store_group.subscribe t.store ~event
