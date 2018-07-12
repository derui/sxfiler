(** Type for current renderer context. User should use this if call RPC, lookup state, or update state. *)
type t = {
  state: Store_group.t;
  rpc: (module Rpc.Rpc);
}
