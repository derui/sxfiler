module Static_registry = Command.Make_registry(struct
    type t = Command.Static_command.t

    let to_name t = t.Command.Static_command.name
  end)
module Dynamic_registry = Command.Make_registry(struct
    type t = (module Command.Instance)

    let to_name (module C: Command.Instance) = C.(Command.name this)
  end)

module type S = sig
  type store

  val rpc : (module Rpc.Rpc)

  val context : (module Context.Instance)

  val store : store

  (** the singleton instance for command registry. *)
  val command_registry : Static_registry.t

  (** the singleton instance for dynamic command registry *)
  val dynamic_command_registry: Dynamic_registry.t
end
