(** Filer_op module defines functions for procedures of filer. *)
open Sxfiler_core

module D = Sxfiler_domain
module U = Sxfiler_usecase
module G = Sxfiler_server_gateway
module T = Sxfiler_rpc.Types
module Jr = Jsonrpc_ocaml_yojson
module Tr = Sxfiler_server_translator
module P = Procedure_intf
module E = Sxfiler_rpc.Endpoints

let make_spec (module Gateway : G.Filer.Make.S) =
  let handle params =
    let%lwt result = Gateway.handle params in
    match result with
    | {Gateway.already_exists = true; _} | {filer = None; _} ->
      Jr.Exception.raise_error Sxfiler_rpc.Errors.Filer.already_exists
    | {filer = Some s; _} -> Lwt.return s
  in
  P.to_procedure ~method_:E.Filer.Make.endpoint
    ~spec:
      P.Spec.
        { params_of_json = `Required Gateway.params_of_yojson
        ; result_to_json = Tr.Filer.to_yojson
        ; handle }

let get_spec (module Gateway : G.Filer.Get.S) =
  let handle params =
    let%lwt result = Gateway.handle params in
    match result with
    | {Gateway.not_found = true; _} | {filer = None; _} ->
      Jsonrpc_ocaml_yojson.(Exception.raise_error Sxfiler_rpc.Errors.Filer.not_found)
    | {filer = Some s; _} -> Lwt.return s
  in
  P.to_procedure ~method_:E.Filer.Get.endpoint
    ~spec:
      P.Spec.
        { params_of_json = `Required Gateway.params_of_yojson
        ; result_to_json = Tr.Filer.to_yojson
        ; handle }

let move_parent_spec (module Gateway : G.Filer.Move_parent.S) =
  let handle params =
    let%lwt result = Gateway.handle params in
    match result with
    | {Gateway.not_found = true; _} | {filer = None; _} ->
      Jsonrpc_ocaml_yojson.(Exception.raise_error Sxfiler_rpc.Errors.Filer.not_found)
    | {filer = Some s; _} -> Lwt.return s
  in
  P.to_procedure ~method_:E.Filer.Move_parent.endpoint
    ~spec:
      P.Spec.
        { params_of_json = `Required Gateway.params_of_yojson
        ; result_to_json = Tr.Filer.to_yojson
        ; handle }

let enter_directory_spec (module Gateway : G.Filer.Enter_directory.S) =
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
  in
  P.to_procedure ~method_:E.Filer.Enter_directory.endpoint
    ~spec:
      P.Spec.
        { params_of_json = `Required Gateway.params_of_yojson
        ; result_to_json = Tr.Filer.to_yojson
        ; handle }

let move_nodes_spec (module Gateway : G.Filer.Move_nodes.S) =
  let handle params =
    let%lwt result = Gateway.handle params in
    let module J = Jsonrpc_ocaml_yojson in
    let module EF = Sxfiler_rpc.Errors.Filer in
    match result with
    | {Gateway.not_found_workbench = true; _} -> J.(Exception.raise_error EF.not_found_workbench)
    | _ -> Lwt.return_unit
  in
  P.to_procedure ~method_:E.Filer.Move_nodes.endpoint
    ~spec:
      P.Spec.
        { params_of_json = `Required Gateway.params_of_yojson
        ; result_to_json = (fun _ -> `Null)
        ; handle }

let delete_nodes_spec (module Gateway : G.Filer.Delete_nodes.S) =
  let handle params =
    let%lwt result = Gateway.handle params in
    let module J = Jsonrpc_ocaml_yojson in
    let module EF = Sxfiler_rpc.Errors.Filer in
    match result with
    | {Gateway.not_found_workbench = true; _} -> J.(Exception.raise_error EF.not_found_workbench)
    | _ -> Lwt.return_unit
  in
  P.to_procedure ~method_:E.Filer.Delete_nodes.endpoint
    ~spec:
      P.Spec.
        { params_of_json = `Required Gateway.params_of_yojson
        ; result_to_json = (fun _ -> `Null)
        ; handle }

let make_procedures (module NS : D.Notification_service.S) : P.procedure list =
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
  let module Get_gateway = G.Filer.Get.Make (U.Filer.Get (Filer_repo)) in
  let module Move_parent_gateway =
    G.Filer.Move_parent.Make
      (U.Filer.Move_parent (Filer_repo) (I.Location_scanner_service) (Global.Clock)) in
  let module Enter_directory_gateway =
    G.Filer.Enter_directory.Make
      (U.Filer.Enter_directory (Filer_repo) (I.Location_scanner_service) (Global.Clock)) in
  let module Move_nodes_gateway =
    G.Filer.Move_nodes.Make
      (U.Filer.Move_nodes.Make (Filer_repo) (Wb_repo) (I.Location_scanner_service)
         (I.Node_transporter_service.Make (NS) (I.Notification_factory))) in
  [ make_spec (module Make_gateway)
  ; get_spec (module Get_gateway)
  ; move_parent_spec (module Move_parent_gateway)
  ; enter_directory_spec (module Enter_directory_gateway)
  ; move_nodes_spec (module Move_nodes_gateway) ]
