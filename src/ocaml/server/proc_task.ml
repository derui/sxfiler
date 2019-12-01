(** Completion module defines functions for Task *)

module G = Sxfiler_server_gateway
module P = Procedure

module Send_reply_spec (G : G.Task.Send_reply.S) : P.Spec = struct
  module Gateway = G

  let method_ = "task/sendReply"
  let param_requirement = `Required
end

module Cancel_spec (G : G.Task.Cancel.S) : P.Spec = struct
  module Gateway = G

  let method_ = "task/cancel"
  let param_requirement = `Required
end
