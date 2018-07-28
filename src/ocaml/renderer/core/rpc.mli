
include module type of (struct include Rpc_intf end)

(** the implementation of {!Client} *)
module Client : Client

(** the implementation of {!Server} *)
module Server : Server
