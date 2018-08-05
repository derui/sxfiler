open Abbrevs

include module type of struct include I.Keymap end

module Make(Client:C.Rpc.Client) : S
