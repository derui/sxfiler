(** This module provides commands for execution. *)
module C = Sxfiler_types.Command

(** Parameter mapping. *)
module Param_map = Map.Make(struct
    type t = string
    let compare = Pervasives.compare
  end)

module type S = sig
  type t

  (** [make ()] returns new instance of commandlet. *)
  val make: unit -> t

  (** [definition] get definition of command.  *)
  val definition:  C.definition

  (** [set_param t ~def ~value] update parameter specified parameter definition [def]. *)
  val set_param: t -> def:C.Param_def.t -> value:Yojson.Safe.json -> t

  (** [plan t] gets a plan for effects if execute this. *)
  val plan: [
    | `No_side_effect
    | `Planning of t -> Sxfiler_types.Node.t list * Sxfiler_types.Node.t list Lwt.t]

  (** [execute t] execute command specified [t]. *)
  val execute: t -> unit Lwt.t
end

(** The module signature for instance of command *)
module type Instance = sig
  module Command : S
  val instance: Command.t
end
