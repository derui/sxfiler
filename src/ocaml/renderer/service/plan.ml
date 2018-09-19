(** Interface for plan service *)
open Abbrevs

module type S = sig
  val reject : E.Plan.Reject.params -> E.Plan.Reject.result Lwt.t

  val plan_move_nodes : E.Plan.Filer.Move_nodes.params -> E.Plan.Filer.Move_nodes.result Lwt.t
  (** [plan_move_nodes param] calls the service to make a plan to move nodes between filers.  *)
end