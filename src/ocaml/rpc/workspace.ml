module T = Sxfiler_types

(** {!Make_sync} module defines interface to make workspace. *)
module Make_sync = struct
  type params = {
    initial_directory: string;
    name: string;
  }

  type result = {
    created: bool;
  }
  let name = "workspace/make/sync"
end

module Get_sync = struct
  type params = {
    name: string;
  }

  type result = T.Workspace.t
  let name = "workspace/get/sync"
end
