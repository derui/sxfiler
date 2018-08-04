(** Task_handler defines handler of task to apply task result to global state. *)
open Sxfiler_core
open Sxfiler_domain
open Sxfiler_server_core
module Rpc = Sxfiler_rpc
module Rpcy = Sxfiler_rpc_yojson

module type S = sig
  type state
  val handle : (module Statable.S with type state = state)
    -> Sxfiler_server_task.Intf.task_result
    -> unit Lwt.t
end

(** Make handler with {module!Sxfiler_domain.Snapshot_record.Clock} abstraction *)
module Make(Clock:Location_record.Clock)
    (Notifier:Notifier.S): S with type state := Root_state.t = struct

  let handle s result =
    let tags = Logger.Tags.module_lib ["result_handler"] in
    match result with
    | `Update_scanner (name', location', nodes) -> begin
        let module S = (val s : Statable.S with type state = Root_state.t) in
        let%lwt () = Logs_lwt.info (fun m -> m ~tags "start handling of {Update_scanner}") in
        let%lwt scanner = S.with_lock (fun state ->
            let scanner = match Root_state.find_scanner ~name:name' state with
              | None -> Scanner.make ~location:location' ~name:name' ~nodes ~history:(Location_history.make ())
              | Some scanner -> Scanner.move_location scanner ~location:location' ~nodes (module Clock)
            in
            let state = Root_state.add_scanner ~scanner state in
            let%lwt () = S.update state in
            Lwt.return scanner
          ) in
        let module Su = Rpc.Notification.Scanner_update in
        let module Suy = Rpcy.Notification.Scanner_update in
        let module Api = (struct
          include Su
          type json = Yojson.Safe.json

          let params_to_json params =
            let open Option.Infix in
            params >|= Suy.params_to_yojson

          let result_of_json _ = ()
        end) in
        let%lwt () = Notifier.notify (module Api) (Some {Su.name = name'; scanner}) in
        Logs_lwt.info (fun m -> m ~tags "finish handling of {Update_scanner}")
      end
    | `Failed err -> Logs_lwt.err @@ fun m -> m "Task error: %s\n" err

end
