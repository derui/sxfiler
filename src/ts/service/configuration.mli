open Abbrevs

include module type of struct
  include Configuration_intf
end

module Make (Client : C.Rpc_client.S) : S
