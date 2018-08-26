open Locator_abbrevs
open C

module type S = sig
  val client : (module Rpc.Client)

  val context : (module Context.Instance)

  val store : S.App.Store.t

  (** the singleton instance for command registry. *)
  val command_registry : Command.Static_registry.t

  (** the singleton instance for dynamic command registry *)
  val dynamic_command_registry: Command.Dynamic_registry.t

  (** Service registry is contains singleton service instances to use in command. *)
  val service_registry: (module Service.Service_registry.S)
end
