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

  let handle s = function
    | `Update_workspace (name, snapshot) -> begin
        let open Lwt in
        let module S = (val s : Statable.S with type state = Root_state.t) in
        S.with_lock (fun state ->
            let ws = match Root_state.find_workspace ~name state with
              | None -> Workspace.make ~current:snapshot ~history:(Snapshot_history.make ())
              | Some ws -> Workspace.replace_current ws ~snapshot ~clock:(module Clock)
            in
            let state = Root_state.add_workspace ~name ~ws state in
            S.update state >>= fun () -> return ws
          )
        >>= fun ws ->
        let module Wu = Rpc.Rpc_notification.Workspace_update in
        let module Wuy = Sxfiler_types_yojson.Rpc.Rpc_notification.Workspace_update in
        let module Api = (struct
          include Wu
          type json = Yojson.Safe.json

          let params_to_json = function
            | None -> None
            | Some params -> Some (Wuy.params_to_yojson params)

          let result_of_json _ = ()
        end) in
        Notifier.notify (module Api) (Some Rpc.Rpc_notification.Workspace_update.{name; workspace = ws})

      end
    | `Failed err -> Lwt_io.eprintf "Task error: %s\n" err

end
