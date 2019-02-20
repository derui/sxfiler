(** Completion module defines functions for RPC of completion. *)

module G = Sxfiler_server_gateway
module P = Procedure

module Setup_spec (G : G.Completion.Setup.S) : P.Spec = struct
  module Gateway = G

  let method_ = "completion/setup"
  let param_requirement = `Required
end

module Read_spec (G : G.Completion.Read.S) : P.Spec = struct
  module Gateway = G

  let method_ = "completion/read"
  let param_requirement = `Required
end
