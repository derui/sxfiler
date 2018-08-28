(** Filer_op module defines functions for procedures of filer. *)
open Sxfiler_core

module D = Sxfiler_domain
module U = Sxfiler_usecase
module G = Sxfiler_server_gateway
module T = Sxfiler_rpc.Types
module Jr = Jsonrpc_ocaml_yojson
module Tr = Sxfiler_server_translator

module Make (Gateway : G.Filer.Make) = struct
  type params = Gateway.params
  type result = T.Filer.t

  let params_of_json = `Required Gateway.params_of_yojson
  let result_to_json = Tr.Filer.to_yojson

  let handle params =
    let%lwt result = Gateway.handle params in
    match result with
    | {Gateway.already_exists = true; _} | {filer = None; _} ->
      Jr.Exception.raise_error Sxfiler_rpc.Errors.Filer.already_exists
    | {filer = Some s; _} ->
      Lwt.return s
end

module Get (Gateway : G.Filer.Get) = struct
  type params = Gateway.params
  type result = T.Filer.t

  let params_of_json = `Required Gateway.params_of_yojson
  let result_to_json = Tr.Filer.to_yojson

  let handle params =
    let%lwt result = Gateway.handle params in
    match result with
    | {Gateway.not_found = true; _} | {filer = None; _} ->
      Jsonrpc_ocaml_yojson.(Exception.raise_error Sxfiler_rpc.Errors.Filer.not_found)
    | {filer = Some s; _} ->
      Lwt.return s
end

module Move_parent (Gateway : G.Filer.Move_parent) = struct
  type params = Gateway.params
  type result = T.Filer.t

  let params_of_json = `Required Gateway.params_of_yojson
  let result_to_json = Tr.Filer.to_yojson

  let handle params =
    let%lwt result = Gateway.handle params in
    match result with
    | {Gateway.not_found = true; _} | {filer = None; _} ->
      Jsonrpc_ocaml_yojson.(Exception.raise_error Sxfiler_rpc.Errors.Filer.not_found)
    | {filer = Some s; _} ->
      Lwt.return s
end

let expose server =
  let module S = Jsonrpc_ocaml_yojson.Server in
  let module W = Sxfiler_usecase.Filer in
  let module I = Sxfiler_server_infra in
  let module Filer_repo = I.Filer_repo.Make (Global.Root) in
  let module Conf_repo = I.Configuration_repo.Make (Global.Root) in
  let module Make_gateway =
    G.Filer.Make (System.Real) (U.Filer.Make (Conf_repo) (Filer_repo) (I.Node_repo))
  in
  let module Make = Procedure_intf.Make (Make (Make_gateway)) in
  let module Get_gateway = G.Filer.Get (U.Filer.Get (Filer_repo)) in
  let module Get = Procedure_intf.Make (Get (Get_gateway)) in
  let module Move_parent_gateway =
    G.Filer.Move_parent (U.Filer.Move_parent (Filer_repo) (I.Node_repo) (Global.Clock)) in
  let module Move_parent = Procedure_intf.Make (Move_parent (Move_parent_gateway)) in
  let module E = Sxfiler_rpc.Endpoints in
  List.fold_left
    (fun server (name, handler) -> S.expose ~_method:name ~handler server)
    server
    [ (E.Filer.Make.endpoint, Make.handler)
    ; (E.Filer.Get.endpoint, Get.handler)
    ; (E.Filer.Move_parent.endpoint, Move_parent.handler) ]
