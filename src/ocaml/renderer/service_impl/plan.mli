open Abbrevs

include module type of struct
  include I.Plan
end

module Make (Client : C.Rpc.Client) : S
