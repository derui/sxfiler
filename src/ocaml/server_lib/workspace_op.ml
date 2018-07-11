(** Workspace_op module defines functions for procedures of workspace. *)
open Sxfiler_server_core
module Runner = Sxfiler_server_task.Runner
module T = Sxfiler_types
module Ty = Sxfiler_types_yojson
module Rpc = Sxfiler_rpc
module Rpcy = Sxfiler_rpc_yojson
module Act = Sxfiler_server_action

module Make_sync(Action:Act.Action_intf.Instance)
    (Root:Statable.S with type state = Root_state.t)
    (Runner:Runner.Instance)
  = Procedure_intf.Make(struct
    include Rpc.Workspace.Make_sync

    let params_of_json = `Required Rpcy.Workspace.Make_sync.params_of_yojson
    let result_to_json = `Result Rpcy.Workspace.Make_sync.result_to_yojson

    let handle param =
      let%lwt state = Root.get () in
      let module S = Sxfiler_server_core.Root_state in
      let%lwt result = match S.find_workspace ~name:param.name state with
        | Some _ -> Lwt.return {created = true}
        | None -> begin
            let module I = Sxfiler_server_task.Intf in
            let module TS = Task.File.Take_snapshot in
            let%lwt _ = Runner.Runner.add_task Runner.instance @@ I.make_instance
                {
                  TS.directory = param.initial_directory;
                  workspace_name = param.name
                }
                (module Action)
                (module TS) in
            Lwt.return {created = false}
          end
      in
      Lwt.return result
  end)

module Get_sync(Action:Act.Action_intf.Instance)
    (Root:Statable.S with type state = Root_state.t) = Procedure_intf.Make(struct
    include Rpc.Workspace.Get_sync

    let params_of_json = `Required Rpcy.Workspace.Get_sync.params_of_yojson
    let result_to_json = `Result Rpcy.Workspace.Get_sync.result_to_yojson

    let handle param =
      let%lwt state = Root.get () in
      let module S = Sxfiler_server_core.Root_state in
      match S.find_workspace ~name:param.name state with
      | Some ws -> Lwt.return ws
      | None -> Jsonrpc_ocaml_yojson.(Exception.raise_error (Types.Error_code.Others (-32000)))
  end)

let expose server =
  let module S = Jsonrpc_ocaml_yojson.Server in
  let module W = Sxfiler_rpc.Workspace in

  let module Runner = (val Global.Task_runner.get (): Runner.Instance) in
  let module Make_sync = Make_sync(Act.Real)(Global.Root)(Runner) in
  let module Get_sync = Get_sync(Act.Real)(Global.Root) in

  List.fold_left (fun server (name, handler) ->
      S.expose ~_method:name ~handler server
    ) server [
    W.Make_sync.name, Make_sync.handler;
    W.Get_sync.name, Get_sync.handler;
  ]
