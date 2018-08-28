open Abbrevs

include module type of struct
  include I.Configuration
end

module Make (Client : C.Rpc.Client) : S
