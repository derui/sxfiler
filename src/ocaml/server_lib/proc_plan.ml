(** Proc_plan module defines functions for procedures of plan. *)

module D = Sxfiler_domain
module U = Sxfiler_usecase
module G = Sxfiler_server_gateway
module T = Sxfiler_rpc.Types
module Jr = Jsonrpc_ocaml_yojson
module Tr = Sxfiler_server_translator
module P = Procedure
module E = Sxfiler_rpc.Endpoints

let reject_spec (module Gateway : G.Plan.Reject.S) =
  let params_of_json = `Required Gateway.params_of_yojson in
  let result_to_json _ = `Null in
  let handle params =
    let%lwt result = Gateway.handle params in
    match result with
    | {Gateway.invalid_id = true; _} ->
      Jsonrpc_ocaml_yojson.(Exception.raise_error Types.Error_code.Invalid_params)
    | {Gateway.unknown_error = true; _} ->
      Jsonrpc_ocaml_yojson.(Exception.raise_error Types.Error_code.Internal_error)
    | _ -> Lwt.return ()
  in
  P.to_procedure ~method_:E.Plan.Reject.endpoint
    ~spec:P.Spec.{params_of_json; result_to_json; handle}

let plan_move_nodes_spec (module Gateway : G.Filer.Plan_move_nodes.S) =
  let params_of_json = `Required Gateway.params_of_yojson in
  let result_to_json = Tr.Plan.to_yojson in
  let handle params =
    let%lwt result = Gateway.handle params in
    let module J = Jsonrpc_ocaml_yojson in
    let module EF = Sxfiler_rpc.Errors.Filer in
    match result with
    | {Gateway.not_found_filer = true; _} -> J.(Exception.raise_error EF.not_found)
    | {plan = None; _} -> J.(Exception.raise_error Types.Error_code.Internal_error)
    | {plan = Some s; _} -> Lwt.return s
  in
  P.to_procedure ~method_:E.Plan.Filer.Move_nodes.endpoint
    ~spec:P.Spec.{params_of_json; result_to_json; handle}

let plan_delete_nodes_spec (module Gateway : G.Filer.Plan_delete_nodes.S) =
  let params_of_json = `Required Gateway.params_of_yojson in
  let result_to_json = Tr.Plan.to_yojson in
  let handle params =
    let%lwt result = Gateway.handle params in
    let module J = Jsonrpc_ocaml_yojson in
    let module EF = Sxfiler_rpc.Errors.Filer in
    match result with
    | {Gateway.not_found_filer = true; _} -> J.(Exception.raise_error EF.not_found)
    | {plan = None; _} -> J.(Exception.raise_error Types.Error_code.Internal_error)
    | {plan = Some s; _} -> Lwt.return s
  in
  P.to_procedure ~method_:E.Plan.Filer.Delete_nodes.endpoint
    ~spec:P.Spec.{params_of_json; result_to_json; handle}

let make_procedures () =
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
  let module Workbench_make = U.Workbench.Make (Filer_repo) (Wb_factory) (Wb_repo) in
  let module Plan_move_nodes_gateway =
    G.Filer.Plan_move_nodes.Make (Workbench_make) (U.Plan.Filer.Move_nodes.Make (Wb_repo))
  in
  let module Plan_delete_nodes_gateway =
    G.Filer.Plan_delete_nodes.Make (Workbench_make) (U.Plan.Filer.Delete_nodes.Make (Wb_repo))
  in
  [ reject_spec (module Reject_gateway)
  ; plan_move_nodes_spec (module Plan_move_nodes_gateway)
  ; plan_delete_nodes_spec (module Plan_delete_nodes_gateway) ]
