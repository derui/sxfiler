(** Workspace_op module defines functions for procedures of workspace. *)
module Runner = Sxfiler_server_task.Runner

module Make_sync = Procedure_intf.Make(struct
    module T = Sxfiler_types
    module Ty = Sxfiler_types_yojson
    include T.Rpc.Rpc_workspace.Make_sync

    let params_of_json = `Required Ty.Rpc.Rpc_workspace.Make_sync.params_of_yojson
    let result_to_json = `Result Ty.Rpc.Rpc_workspace.Make_sync.result_to_yojson

    let handle param =
      let%lwt state = Global.Root.get () in
      let module S = Sxfiler_server_core.Root_state in
      let%lwt result = match S.find_workspace ~name:param.name state with
        | Some _ -> Lwt.return {created = true}
        | None -> begin
            let module I = Sxfiler_server_task.Intf in
            let module TS = Task.File.Take_snapshot in
            let%lwt _ = Runner.add_task @@ I.make_instance
                TS.({directory = param.initial_directory;
                     workspace_name = param.name
                    })
                (module Sxfiler_server_action.Real)
                (module TS) in
            Lwt.return {created = false}
          end
      in
      Lwt.return result
  end)

let expose server =
  let module S = Jsonrpc_ocaml_yojson.Server in
  let module R = Sxfiler_types.Rpc.Rpc_workspace in

  List.fold_left (fun server (name, handler) ->
      S.expose ~_method:name ~handler server
    ) server [
    (R.Make_sync.name, Make_sync.handler);
  ]
