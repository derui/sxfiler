(** Workspace_op module defines functions for procedures of workspace. *)
module Rpc = Jsonrpc_ocaml_yojson
module Runner = Sxfiler_server_task.Runner

(** prefixes of completion module. *)
let module_prefixes = ["workspace"]

module Make_sync = Procedure_intf.Make(struct
    module T = Sxfiler_types
    module Ty = Sxfiler_types_yojson
    include T.Rpc.Workspace.Make_sync

    let param_of_json = `Required Ty.Rpc.Workspace.Make_sync.param_of_yojson
    let result_to_json = `Result Ty.Rpc.Workspace.Make_sync.result_to_yojson

    let handle param =
      let%lwt state = State.get_current_state () in
      let module S = Sxfiler_server_core.State in
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

  List.fold_left (fun server (name, handler) ->
      S.expose ~_method:Util.(make_method ~module_prefixes ~name) ~handler server
    ) server [
    ("make/sync", Make_sync.handler);
  ]
