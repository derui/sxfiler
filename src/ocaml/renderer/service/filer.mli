open Abbrevs

include module type of struct
  include Filer_intf
end

module Make (Client : C.Rpc.Client) : S
