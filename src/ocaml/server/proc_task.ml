(** Completion module defines functions for Task *)

module G = Sxfiler_server_gateway
module P = Procedure

module Send_interaction_spec (G : G.Task.Send_interaction.S) : P.Spec = struct
  module Gateway = G

  let method_ = "task/sendInteraction"
  let param_requirement = `Required
end
