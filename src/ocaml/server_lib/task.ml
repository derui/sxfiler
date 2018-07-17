(** This module defines tasks are based on {!Task_intf} *)

module T = Sxfiler_types
open Sxfiler_server_task.Intf

module Scanner = struct
  module Move = struct
    type t = {
      name: string;
      location: string;
    }

    module Task : S with type params = t = struct
      type params = t

      let plan = `No_plan
      let apply _ params action =
        let module Action = (val action : A.Instance) in
        let location = Action.No_side_effect.resolve_realpath params.location in
        let%lwt snapshot = Action.No_side_effect.read_dir ~directory:location in
        Lwt.return @@ `Update_scanner (params.name, location, snapshot)
    end

    include Task
  end
end
