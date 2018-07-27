
module type S = sig
  type store
  type message

  module Rpc : Rpc_intf.Rpc

  module Context: Context_intf.Instance with type store := store
                                         and type message := message

  module Repository : sig
    module Scanner : Repository_intf.Scanner_instance
    module Keybindings: Repository_intf.Keybindings_instance
    module Configuration: Repository_intf.Configuration_instance
  end
end
