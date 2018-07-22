(** This module defines some RPC to initialize command, to set parameter value, to execute it. *)
module T = Sxfiler_types

(** {!Init_sync} defines interface to initialize the command specified in parameter. *)
module Init_sync = struct
  type params = {
    command: T.Command.Class.t;
  }

  type result = unit
  let name = "command/init/sync"
end

(** {!Param_update} defines interface to update a parameter with given value.
    Parameter will have different types for each, so [value] type in this module should abstract type in definition.
*)
module Param_update = struct
  module type Value = sig
    type t
  end

  module type S = sig
    type value
    type params = {
      name: string;
      value: value;
    }

    type result = unit
    val name: string
  end

  module Make(V:Value) : S with type value := V.t = struct
    type params = {
      name: string;
      value: V.t;
    }

    type result = unit
    let name = "command/param/update"
  end
end
