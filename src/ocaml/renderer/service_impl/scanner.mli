open Abbrevs

include module type of struct include I.Scanner end

module Make(Client:C.Rpc.Client) : S
