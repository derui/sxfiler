module T = Sxfiler_types

type ('a, 'b) executor = 'a -> (module Context.Instance) -> 'b Lwt.t
type execution_plan = [
  | `No_plan
  | `Plan of ((string * string) list, unit) executor
]

(** type for static command. *)
module Static_command = struct
  type t = {
    name: string;
    execute_plan: execution_plan;
    executor: ((string * string) list, unit) executor;
  }
end

(** The signature of completer is used from omni bar with dynamic command.  *)
module type Completer = sig
  type t

  (** [read t input] returns collection to be used to show list as candidates. *)
  val read: t -> (string, T.Completion.collection) executor
end

(** The signature of static command called from key binding *)
module type Dynamic_command = sig
  type t

  (** [name] is the name of this command. This should be unique from
      all commands.
  *)
  val name: t -> string

  (** To enable completer or not. If [completer t] returns [`No_completion], command runner will not
      do anything for completion.
  *)
  val completer: [
    | `No_completion
    | `Complete of t -> (module Completer)
  ]

  (** Definition of plan to get difference of between before and after
      command execution.
  *)
  val plan: [
    | `No_plan
    | `Plan of t -> (string, unit) executor
  ]

  (** [execute t params context] returns behavior that execute command with
      some of dependencies.
  *)
  val execute: t -> (string, unit) executor

end

(** Signature for dynamic command *)
module type Instance = sig
  module Command: Dynamic_command
  val this : Command.t
end
