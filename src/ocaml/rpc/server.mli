open Abbrev

include module type of struct
  include Server_intf
end

val make : (module I.Ws_actor.Instance) -> (module Instance)
(** [make (module Actor)] make a new instance of [S] that is RPC Server *)
