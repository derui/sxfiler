(** This module defines tasks are based on {!Task_intf} *)

module T = Sxfiler_types
open Sxfiler_server_task.Intf

module File = struct
  module Take_snapshot = struct
    type t = {
      directory: string;
      workspace_name: string;
    }

    module Task : S with type params = t = struct
      type params = t

      let plan = `No_plan
      let apply _ params action =
        let module Action = (val action : A.Instance) in
        let open Lwt in
        Action.No_side_effect.take_snapshot ~directory:params.directory
        >|= fun snapshot -> `Update_workspace (params.workspace_name, snapshot)
    end

    include Task
  end
end
