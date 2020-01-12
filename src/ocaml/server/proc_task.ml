(** Completion module defines functions for Task *)

module G = Sxfiler_server_gateway
module P = Procedure

module Reply_to_overwrite_spec (G : G.Task.Reply_to_overwrite.S) : P.Spec = struct
  module Gateway = G

  let method_ = "task/reply/overwrite"
  let param_requirement = `Required
end

module Reply_to_rename_spec (G : G.Task.Reply_to_rename.S) : P.Spec = struct
  module Gateway = G

  let method_ = "task/reply/rename"
  let param_requirement = `Required
end

module Cancel_spec (G : G.Task.Cancel.S) : P.Spec = struct
  module Gateway = G

  let method_ = "task/cancel"
  let param_requirement = `Required
end
