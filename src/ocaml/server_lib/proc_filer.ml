(** Filer_op module defines functions for procedures of filer. *)
open Sxfiler_core

module D = Sxfiler_domain
module U = Sxfiler_usecase
module G = Sxfiler_server_gateway
module T = Sxfiler_rpc.Types
module Jr = Jsonrpc_ocaml_yojson
module Tr = Sxfiler_server_translator

module Make (Gateway : G.Filer.Make.S) = struct
  type params = Gateway.params
  type result = T.Filer.t

  let params_of_json = `Required Gateway.params_of_yojson
  let result_to_json = Tr.Filer.to_yojson

  let handle params =
    let%lwt result = Gateway.handle params in
    match result with
    | {Gateway.already_exists = true; _} | {filer = None; _} ->
      Jr.Exception.raise_error Sxfiler_rpc.Errors.Filer.already_exists
    | {filer = Some s; _} -> Lwt.return s
end

module Get (Gateway : G.Filer.Get.S) = struct
  type params = Gateway.params
  type result = T.Filer.t

  let params_of_json = `Required Gateway.params_of_yojson
  let result_to_json = Tr.Filer.to_yojson

  let handle params =
    let%lwt result = Gateway.handle params in
    match result with
    | {Gateway.not_found = true; _} | {filer = None; _} ->
      Jsonrpc_ocaml_yojson.(Exception.raise_error Sxfiler_rpc.Errors.Filer.not_found)
    | {filer = Some s; _} -> Lwt.return s
end

module Move_parent (Gateway : G.Filer.Move_parent.S) = struct
  type params = Gateway.params
  type result = T.Filer.t

  let params_of_json = `Required Gateway.params_of_yojson
  let result_to_json = Tr.Filer.to_yojson

  let handle params =
    let%lwt result = Gateway.handle params in
    match result with
    | {Gateway.not_found = true; _} | {filer = None; _} ->
      Jsonrpc_ocaml_yojson.(Exception.raise_error Sxfiler_rpc.Errors.Filer.not_found)
    | {filer = Some s; _} -> Lwt.return s
end

module Enter_directory (Gateway : G.Filer.Enter_directory.S) = struct
  type params = Gateway.params
  type result = T.Filer.t

  let params_of_json = `Required Gateway.params_of_yojson
  let result_to_json = Tr.Filer.to_yojson

  let handle params =
    let%lwt result = Gateway.handle params in
    let module J = Jsonrpc_ocaml_yojson in
    let module EF = Sxfiler_rpc.Errors.Filer in
    match result with
    | {Gateway.not_found_filer = true; _} -> J.(Exception.raise_error EF.not_found)
    | {Gateway.not_found_node = true; _} -> J.(Exception.raise_error EF.not_found_node)
    | {Gateway.not_directory = true; _} -> J.(Exception.raise_error EF.not_directory)
    | {filer = None; _} -> J.(Exception.raise_error Types.Error_code.Internal_error)
    | {filer = Some s; _} -> Lwt.return s
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
  let module Make_gateway =
    G.Filer.Make.Make
      (System.Real)
      (U.Filer.Make (Conf_repo) (Filer_repo) (I.Location_scanner_service))
  in
  let module Make = Procedure_intf.Make (Make (Make_gateway)) in
  let module Get_gateway = G.Filer.Get.Make (U.Filer.Get (Filer_repo)) in
  let module Get = Procedure_intf.Make (Get (Get_gateway)) in
  let module Move_parent_gateway =
    G.Filer.Move_parent.Make
      (U.Filer.Move_parent (Filer_repo) (I.Location_scanner_service) (Global.Clock)) in
  let module Move_parent = Procedure_intf.Make (Move_parent (Move_parent_gateway)) in
  let module Enter_directory_gateway =
    G.Filer.Enter_directory.Make
      (U.Filer.Enter_directory (Filer_repo) (I.Location_scanner_service) (Global.Clock)) in
  let module Enter_directory = Procedure_intf.Make (Enter_directory (Enter_directory_gateway)) in
  let module Plan_move_nodes_gateway =
    G.Filer.Plan_move_nodes.Make
      (U.Workbench.Make (Filer_repo) (Wb_factory) (Wb_repo)) (U.Filer.Plan_move_nodes (Wb_repo))
  in
  let module Plan_move_nodes = Procedure_intf.Make (Plan_move_nodes (Plan_move_nodes_gateway)) in
  let module E = Sxfiler_rpc.Endpoints in
  List.fold_left
    (fun server (name, handler) -> S.expose ~_method:name ~handler server)
    server
    [ (E.Filer.Make.endpoint, Make.handler)
    ; (E.Filer.Get.endpoint, Get.handler)
    ; (E.Filer.Move_parent.endpoint, Move_parent.handler)
    ; (E.Filer.Enter_directory.endpoint, Enter_directory.handler)
    ; (E.Filer.Plan_move_nodes.endpoint, Plan_move_nodes.handler) ]
