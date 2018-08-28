open Abbrevs

include module type of struct
  include I.Filer
end

module Make (Client : C.Rpc.Client) : S
