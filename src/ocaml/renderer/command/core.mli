(** This module defines base signatures for command and registry of it.

    All commands in this application must define with signatures in this module, and
    must fulfill requirements of signatures.
*)
include module type of struct
  include Core_intf
end

module Make_registry (Com : Registry.Command) : Registry.S with type command := Com.t
module Static_registry : Registry.S with type command := Static_command.t
module Dynamic_registry : Registry.S with type command := (module Instance)
