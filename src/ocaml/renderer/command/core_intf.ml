module C = Sxfiler_renderer_core
module Co = Sxfiler_domain.Completion
module S = Sxfiler_renderer_store
module P = Sxfiler_renderer_background.Planner

type command_args = (string * string) list
type ('a, 'state, 'b) executor = 'a -> 'state -> (module C.Context.Instance) -> 'b Lwt.t

(* The type of function that is to execute command body with
   application state.
*)
type ('a, 'state) command =
  | Immediate of ('a, 'state, unit) executor
  | With_plan of ('a -> (module C.Context.Instance) -> P.executor)

(** type for static command. *)
type static_command =
  { name : string
  ; executor : (command_args, S.App.State.t) command }

(** The interface to run command *)
module type Runner = sig
  type command
  type param
  type state

  val run :
    param:param -> state:state -> context:(module C.Context.Instance) -> command -> unit Lwt.t
    (** [run ~param ~state ~context command] run [command] with [param] and [state]. *)
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

  val execute : t -> (string, state) command
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
