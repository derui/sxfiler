(** This module defines base signatures for command and registry of it.

    All commands in this application must define with signatures in this module, and
    must fulfill requirements of signatures.
*)
include module type of struct
  include Core_intf
end

module Make_registry (Com : Registry.Command) : Registry.S with type command := Com.t
module Static_registry : Registry.S with type command := static_command
module Dynamic_registry : Registry.S with type command := (module Instance)

module Static_command_runner :
  Runner
  with type command := static_command
   and type state := S.App.State.t
   and type param := command_args
