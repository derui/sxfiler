include module type of struct
  include Rpc_intf
end

(** the implementation of {!Client} *)
module Make_client (Rpc : Rpc) : Client

(** the implementation of {!Server} *)
module Server : Server
