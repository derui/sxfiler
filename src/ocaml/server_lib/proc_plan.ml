(** Proc_plan module defines functions for procedures of plan. *)

module D = Sxfiler_domain
module U = Sxfiler_usecase
module G = Sxfiler_server_gateway
module T = Sxfiler_rpc.Types
module Jr = Jsonrpc_ocaml_yojson
module Tr = Sxfiler_server_translator

module Reject (Gateway : G.Plan.Reject.S) = struct
  type params = Gateway.params
  type result = unit

  let params_of_json = `Required Gateway.params_of_yojson
  let result_to_json _ = `Null

  let handle params =
    let%lwt result = Gateway.handle params in
    match result with
    | {Gateway.invalid_id = true; _} ->
      Jsonrpc_ocaml_yojson.(Exception.raise_error Types.Error_code.Invalid_params)
    | {Gateway.unknown_error = true; _} ->
      Jsonrpc_ocaml_yojson.(Exception.raise_error Types.Error_code.Internal_error)
    | _ -> Lwt.return ()
end

module Plan_move_nodes (Gateway : G.Filer.Plan_move_nodes.S) = struct
  type params = Gateway.params
  type result = T.Plan.t

  let params_of_json = `Required Gateway.params_of_yojson
  let result_to_json = Tr.Plan.to_yojson

  let handle params =
    let%lwt result = Gateway.handle params in
    let module J = Jsonrpc_ocaml_yojson in
    let module EF = Sxfiler_rpc.Errors.Filer in
    match result with
    | {Gateway.not_found_filer = true; _} -> J.(Exception.raise_error EF.not_found)
    | {plan = None; _} -> J.(Exception.raise_error Types.Error_code.Internal_error)
    | {plan = Some s; _} -> Lwt.return s
end

let expose server =
  let module S = Jsonrpc_ocaml_yojson.Server in
  let module W = Sxfiler_usecase.Filer in
  let module I = Sxfiler_server_infra in
  let module Filer_repo = I.Filer_repo.Make (Global.Root) in
  let module Conf_repo = I.Configuration_repo.Make (Global.Root) in
  let module Wb_repo = I.Workbench_repo.Make (Global.Workbench) in
  let module Wb_factory = I.Workbench_factory.Make (struct
      let get = Random.get_state
    end) in
  let module Reject_gateway = G.Plan.Reject.Make (U.Plan.Reject.Make (Wb_repo)) in
  let module Reject = Procedure_intf.Make (Reject (Reject_gateway)) in
  let module Plan_move_nodes_gateway =
    G.Filer.Plan_move_nodes.Make
      (U.Workbench.Make (Filer_repo) (Wb_factory) (Wb_repo))
      (U.Plan.Filer.Move_nodes.Make (Wb_repo))
  in
  let module Plan_move_nodes = Procedure_intf.Make (Plan_move_nodes (Plan_move_nodes_gateway)) in
  let module E = Sxfiler_rpc.Endpoints in
  List.fold_left
    (fun server (name, handler) -> S.expose ~_method:name ~handler server)
    server
    [ (E.Plan.Reject.endpoint, Reject.handler)
    ; (E.Plan.Filer.Move_nodes.endpoint, Plan_move_nodes.handler) ]
