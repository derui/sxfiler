(** This module defines tasks are based on {!Task_intf} *)

module T = Sxfiler_types

include Sxfiler_server_task.Intf

module File = struct
  module Take_snapshot = struct
    type t = {
      directory: string;
      stack_name: string;
    }

    module Task : S with type params = t = struct
      type params = t

      let plan = `No_plan
      let apply state params action =
        let module Action = (val action : A.Instance) in
        let open Lwt in
        Action.No_side_effect.take_snapshot ~directory:params.directory
        >|= fun snapshot -> `Update_stack (params.stack_name, T.Tree_stack.Snapshot snapshot)
    end
  end
end
