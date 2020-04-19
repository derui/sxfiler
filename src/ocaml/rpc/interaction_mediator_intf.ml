open Sxfiler_domain
module F = Sxfiler_workflow

(** module signature of Runner. *)
module type S = sig
  type t

  val require_action : t -> command:Interaction.command -> Interaction.event Lwt.t
  (** [require_action t ~command] require the command to user and wait user decision. *)
end

(** the sigunature for Instance of Runner. *)
module type Instance = sig
  module Mediator : S

  val instance : Mediator.t
end
