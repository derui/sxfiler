module Rpc = Sxfiler_rpc

(** This module defines rpc interface to get current key bindings.
    Replace [json] on implementation to match rpc.
*)
module Get_sync = Rpc.Keybindings.Get_sync.Make(struct
    type t = < > Js.t
  end)
