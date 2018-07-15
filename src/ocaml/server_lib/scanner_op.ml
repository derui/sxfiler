(** Scanner_op module defines functions for procedures of scanner. *)
open Sxfiler_server_core
module Runner = Sxfiler_server_task.Runner
module T = Sxfiler_types
module Ty = Sxfiler_types_yojson
module Rpc = Sxfiler_rpc
module Rpcy = Sxfiler_rpc_yojson
module Act = Sxfiler_server_action

module Make_sync
    (Action:Act.Action_intf.Instance)
    (Root:Statable.S with type state = Root_state.t)
    (Runner:Runner.Instance)
  = Procedure_intf.Make(struct
    include Rpc.Scanner.Make_sync

    let params_of_json = `Required Rpcy.Scanner.Make_sync.params_of_yojson
    let result_to_json = `Void

    let handle param =
      let%lwt state = Root.get () in
      let module S = Sxfiler_server_core.Root_state in
      let%lwt result = match S.find_scanner ~name:param.name state with
        (* raise error if scanner already exists. *)
        | Some _ -> Jsonrpc_ocaml_yojson.Exception.raise_error Rpc.Errors.Scanner.already_exists
        | None -> begin
            let module I = Sxfiler_server_task.Intf in
            let module Task = Task.Scanner.Move in
            let%lwt _ = Runner.Runner.add_task Runner.instance @@ I.make_instance
                {
                  Task.location = param.initial_location;
                  name = param.name
                }
                (module Action)
                (module Task) in
            Lwt.return_unit
          end
      in
      Lwt.return result
  end)

module Get_sync(Action:Act.Action_intf.Instance)
    (Root:Statable.S with type state = Root_state.t) = Procedure_intf.Make(struct
    include Rpc.Scanner.Get_sync

    let params_of_json = `Required Rpcy.Scanner.Get_sync.params_of_yojson
    let result_to_json = `Result Rpcy.Scanner.Get_sync.result_to_yojson

    let handle param =
      let%lwt state = Root.get () in
      let module S = Sxfiler_server_core.Root_state in
      match S.find_scanner ~name:param.name state with
      | Some scanner -> Lwt.return scanner
      | None -> Jsonrpc_ocaml_yojson.(Exception.raise_error Rpc.Errors.Scanner.not_found)
  end)

let expose server =
  let module S = Jsonrpc_ocaml_yojson.Server in
  let module W = Sxfiler_rpc.Scanner in

  let module Runner = (val Global.Task_runner.get (): Runner.Instance) in
  let module Make_sync = Make_sync(Act.Real)(Global.Root)(Runner) in
  let module Get_sync = Get_sync(Act.Real)(Global.Root) in

  List.fold_left (fun server (name, handler) ->
      S.expose ~_method:name ~handler server
    ) server [
    W.Make_sync.name, Make_sync.handler;
    W.Get_sync.name, Get_sync.handler;
  ]
