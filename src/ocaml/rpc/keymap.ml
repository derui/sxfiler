open Sxfiler_domain

(** This module defines rpc interface to get current key bindings.
    Replace [json] on implementation to match rpc.
*)
module Get_sync = struct
  type params = unit

  type result = string Key_map.t
  let name = "keymap/get/sync"
end
