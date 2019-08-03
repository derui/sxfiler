(** Filer_op module defines functions for procedures of filer. *)

module G = Sxfiler_server_gateway
module P = Procedure

module Make_spec (Gateway : G.Filer.Make.S) : P.Spec = struct
  module Gateway = Gateway

  let method_ = "filer/make"
  let param_requirement = `Required
end

module Get_spec (Gateway : G.Filer.Get.S) : P.Spec = struct
  module Gateway = Gateway

  let method_ = "filer/get"
  let param_requirement = `Required
end

module Move_parent_spec (Gateway : G.Filer.Move_parent.S) : P.Spec = struct
  module Gateway = Gateway

  let method_ = "filer/moveParent"
  let param_requirement = `Required
end

module Enter_directory_spec (Gateway : G.Filer.Enter_directory.S) : P.Spec = struct
  module Gateway = Gateway

  let method_ = "filer/enterDirectory"
  let param_requirement = `Required
end

module Toggle_mark_spec (Gateway : G.Filer.Toggle_mark.S) : P.Spec = struct
  module Gateway = Gateway

  let method_ = "filer/toggleMark"
  let param_requirement = `Required
end

module Move_spec (Gateway : G.Filer.Move.S) : P.Spec = struct
  module Gateway = Gateway

  let method_ = "filer/move"
  let param_requirement = `Required
end

module Copy_spec (Gateway : G.Filer.Copy.S) : P.Spec = struct
  module Gateway = Gateway

  let method_ = "filer/copy"
  let param_requirement = `Required
end

module Delete_spec (Gateway : G.Filer.Delete.S) : P.Spec = struct
  module Gateway = Gateway

  let method_ = "filer/delete"
  let param_requirement = `Required
end

module Jump_location_spec (Gateway : G.Filer.Jump_location.S) : P.Spec = struct
  module Gateway = Gateway

  let method_ = "filer/jump"
  let param_requirement = `Required
end
