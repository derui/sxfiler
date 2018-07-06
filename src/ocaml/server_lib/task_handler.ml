(** Task_handler defines handler of task to apply task result to global state. *)
open Sxfiler_types
open Sxfiler_server_core

module type S = sig
  type state
  val handle : (module Statable.S with type state = state)
    -> Sxfiler_server_task.Intf.task_result
    -> unit Lwt.t
end

(** Make handler with {module!Sxfiler_types.Snapshot_record.Clock} abstraction *)
module Make(Clock:Snapshot_record.Clock)
  (Notifier:Notifier.S): S with type state := Root_state.t = struct

  let handle state = function
    | `Update_workspace (name, snapshot) -> begin
        let open Lwt in
        let module S = (val state : Statable.S with type state = Root_state.t) in
        S.with_lock (fun state ->
            let ws = match Root_state.find_workspace ~name state with
              | None -> Workspace.make ~current:snapshot ~history:(Snapshot_history.make ())
              | Some ws -> Workspace.replace_current ws ~snapshot ~clock:(module Clock)
            in
            let state = Root_state.add_workspace ~name ~ws state in
            S.update state
          )
          >>= fun () -> Notifier.notify

      end
    | `Failed err -> Lwt_io.eprintf "Task error: %s\n" err

end
