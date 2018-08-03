(** This module defines base signatures for command and registry of it.

    All commands in this application must define with signatures in this module, and
    must fulfill requirements of signatures.
*)
include module type of (struct include Command_intf end)

module Make_registry(Com:Registry.Command) : Registry.S with type command := Com.t
