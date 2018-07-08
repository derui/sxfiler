(** Task_handler defines handler of task to apply task result to global state. *)
open Sxfiler_core
open Sxfiler_types
open Sxfiler_server_core
module Rpc = Sxfiler_rpc
module Rpcy = Sxfiler_rpc_yojson

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
    | `Update_workspace (name', snapshot) -> begin
        let module S = (val s : Statable.S with type state = Root_state.t) in
        let%lwt ws = S.with_lock (fun state ->
            let ws = match Root_state.find_workspace ~name:name' state with
              | None -> Workspace.make ~current:snapshot ~history:(Snapshot_history.make ())
              | Some ws -> Workspace.replace_current ws ~snapshot ~clock:(module Clock)
            in
            let state = Root_state.add_workspace ~name:name' ~ws state in
            let%lwt () = S.update state in
            Lwt.return ws
          ) in
        let module Wu = Rpc.Notification.Workspace_update in
        let module Wuy = Rpcy.Notification.Workspace_update in
        let module Api = (struct
          include Wu
          type json = Yojson.Safe.json

          let params_to_json params =
            let open Option.Infix in
            params >|= Wuy.params_to_yojson

          let result_of_json _ = ()
        end) in
        Notifier.notify (module Api) (Some Wu.{name = name'; workspace = ws})

      end
    | `Failed err -> Lwt_io.eprintf "Task error: %s\n" err

end
