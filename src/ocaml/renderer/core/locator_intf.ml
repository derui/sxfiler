
module type S = sig
  type store

  val rpc : (module Rpc.Rpc)

  val context : (module Context.Instance)

  val store : store

  (** the singleton instance for command registry. *)
  val command_registry : Command.Registry.t
end
