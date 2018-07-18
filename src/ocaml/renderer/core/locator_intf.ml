
module type S = sig
  module Rpc : Rpc_intf.Rpc

  module Repository : sig
    module Scanner : Repository_intf.Scanner_instance
    module Keybindings: Repository_intf.Keybindings_instance
  end
end
