(** Type for current renderer context. User should use this if call RPC, lookup state, or update state. *)
module C = Sxfiler_renderer_core

type t = {
  store: C.Store_group.t;
  rpc: (module Rpc.Rpc);
}

(** [get_store t ~tag] is helper function to get store directly.  *)
let get_store t ~tag = C.Store_group.get t.store ~tag
