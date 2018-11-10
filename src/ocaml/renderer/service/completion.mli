open Abbrevs

include module type of struct
  include Completion_intf
end

module Make (Client : C.Rpc.Client) : S
