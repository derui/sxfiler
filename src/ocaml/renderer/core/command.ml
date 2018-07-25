(** {!Command} defines types for command pallet. *)
module Class = struct
  type t =
    | Scanner_jump
end

(** {!Param_spec} defines type to define specification of parameter of the command. *)
module Param_spec = struct
  type interface =
    | Arbitrarily
    | Select
    | Multi_select

  type t = {
    name: string;
    interface: interface;
  }
end

(** Command_def allow to define an interface of a command. *)
module Command_def = struct
  type t = {
    command_class: Class.t;
    parameters: Param_spec.t list;
  }
end
