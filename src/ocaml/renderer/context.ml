module C = Sxfiler_common

(** Type for current renderer context. User should use this if call RPC, lookup state, or update state. *)
type t = {
  state: C.State.t;
  rpc: Rpc.t;
}
