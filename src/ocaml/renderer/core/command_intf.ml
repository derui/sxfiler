module T = Sxfiler_types

(** {!Command} defines types for command pallet. *)
module Class = struct
  type t =
    | Scanner_jump
end

(** modules for parameter specifications of a command.  *)
module Param_spec = struct
  type interface =
    | Arbitrarily
    | Select
    | Multi_select
  type completion =
    | Current_scanner
    | History
    | Commands

  module type S = sig
    (** [name] get the name of parameter. *)
    val name: string

    (** [interface] get the interface of parameter to define the type of
        user interaction interface.
    *)
    val interface: interface

    (** [setup_completion locator] get initial source of completion. Return [`No_completion] if do not need completion. *)
    val setup_completion: [
      | `No_completion
      | `Completion of completion
    ]
  end
end

module type S = sig
  type t

  type store

  (** [name] is the name of this command. This should be unique from
      all commands. *)
  val name: t -> string

  (** [param_defs] is the specifications of parameter of this command.
      Order of this definitions is important to declare order of parameter.
  *)
  val param_defs: t -> (module Param_spec.S) list

  (** Definition of plan to get difference of between before and after
      command execution.
  *)
  val plan: [
    | `No_plan
    | `Plan of t
        -> (string * string) list
        -> (module Context.Instance)
        -> unit Lwt.t
  ]

  (** [execute t params] returns behavior that execute command with
      some of dependencies.
  *)
  val execute: t
    -> (string * string) list
    -> (module Context.Instance)
    -> unit Lwt.t

end

(** Signature for command *)
module type Instance = sig
  type store
  module Command: S with type store = store
  val this : Command.t
end
