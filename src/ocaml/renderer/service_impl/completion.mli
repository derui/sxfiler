open Abbrevs

include module type of struct include I.Completion end

module Make(Client:C.Rpc.Client) : S
