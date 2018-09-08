(** Interface for filer service *)
open Abbrevs

module type S = sig
  val make : E.Filer.Make.params -> (E.Filer.Make.result, [`Already_exists]) result Lwt.t
  (** [make param] calls the service to make filer with [param] .
      All exceptions raised from this are below. You can catch exceptions via {!Lwt.catch}
  *)

  val get : E.Filer.Get.params -> (E.Filer.Get.result, [`Not_found]) result Lwt.t
  (** [get params] calls the service to get filer specified [params].
      If filer not found, return {!Error} with variants.
  *)

  val move_parent : E.Filer.Move_parent.params -> E.Filer.Move_parent.result Lwt.t
  (** [move_parent param] calls the service to move filer to parent location of it.

      @raise Sxfiler_core.Error.t if service failed with some of errors.
  *)

  val enter_directory : E.Filer.Enter_directory.params -> E.Filer.Enter_directory.result Lwt.t
  (** [enter_directory param] calls the service to enter directory to specified directory of node in a filer.

      @raise Sxfiler_core.Error.t if service failed with some of errors.
  *)

  val plan_move_nodes : E.Filer.Plan_move_nodes.params -> E.Filer.Plan_move_nodes.result Lwt.t
  (** [plan_move_nodes param] calls the service to make a plan to move nodes between filers.  *)
end
