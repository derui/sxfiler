(** Proc_plan module defines functions for procedures of plan. *)

module G = Sxfiler_server_gateway

module Reject_spec (G : G.Plan.Reject.S) : Procedure.Spec = struct
  module Gateway = G

  let method_ = "plan/reject"
  let param_requirement = `Required
end

module Make_move_plan_spec (G : G.Plan.Filer.Make_move_plan.S) : Procedure.Spec = struct
  module Gateway = G

  let method_ = "plan/filer/moveNodes"
  let param_requirement = `Required
end

module Make_delete_plan_spec (G : G.Plan.Filer.Make_delete_plan.S) : Procedure.Spec = struct
  module Gateway = G

  let method_ = "plan/filer/deleteNodes"
  let param_requirement = `Required
end
