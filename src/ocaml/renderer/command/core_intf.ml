module C = Sxfiler_renderer_core
module Co = Sxfiler_completion.Domain
module S = Sxfiler_renderer_store

type command_args = (string * string) list
(* The type of function that is to execute command body with
   application state.
*)
type ('a, 'state, 'b) executor = 'a -> 'state -> (module C.Context.Instance) -> 'b Lwt.t
type 'store execution_plan = [`No_plan | `Plan of (command_args, 'store, unit) executor]

(** type for static command. *)
module Static_command = struct
  type state = S.App.State.t

  type t =
    { name : string
    ; execute_plan : state execution_plan
    ; executor : (command_args, state, unit) executor }
end

(** The signature of completer is used from omni bar with dynamic command.  *)
module type Completer = sig
  type t
  type state = S.App.State.t

  val read : t -> (string, state, Co.collection) executor
  (** [read t input] returns collection to be used to show list as candidates. *)
end

(** The signature of static command called from key binding *)
module type Dynamic_command = sig
  type t
  type state = S.App.State.t

  val name : t -> string
  (** [name] is the name of this command. This should be unique from
      all commands.
  *)

  val completer : [`No_completion | `Complete of t -> (module Completer)]
  (** To enable completer or not. If [completer t] returns [`No_completion], command runner will not
      do anything for completion.
  *)

  val plan : [`No_plan | `Plan of t -> (string, state, unit) executor]
  (** Definition of plan to get difference of between before and after
      command execution.
  *)

  val execute : t -> (string, state, unit) executor
  (** [execute t params context] returns behavior that execute command with
      some of dependencies.
  *)
end

(** Signature for dynamic command *)
module type Instance = sig
  module Command : Dynamic_command

  val this : Command.t
end

module Registry = struct
  module type Command = sig
    type t

    val to_name : t -> string
    (** [to_name t] should return the name of the command [t] *)
  end

  module type S = sig
    type t
    type command

    val make : unit -> t
    (** [make ()] returns new instance of Registry *)

    val register : t -> command -> t
    (** [register t command] add a command to registry [t]. Overwrite old command if give the command that is same name. *)

    val get : t -> name:string -> command option
    (** [get t ~name] returns the command having [name]. *)

    val names : t -> string list
    (** [names t] returns list that contains actions already registered *)
  end
end
