module T = Sxfiler_types

(** This module defines rpc interface to manage application configuration.
*)
module Get_sync = struct
  type params = unit

  type result = T.Configuration.t
  let name = "configuration/get/sync"
end
