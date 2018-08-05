(** Scanner_op module defines functions for procedures of scanner. *)
open Sxfiler_core
module D = Sxfiler_domain
module U = Sxfiler_usecase
module G = Sxfiler_server_gateway
module T = Sxfiler_server_translator
module Jr = Jsonrpc_ocaml_yojson

module Make(Gateway: G.Scanner.Make) = struct
  type params = Gateway.params

  type result = T.Scanner.t

  let params_of_json = `Required Gateway.params_of_yojson
  let result_to_json = `Result T.Scanner.to_yojson

  let handle params =
    let%lwt result = Gateway.handle params in
    match result with
    | {Gateway.already_exists = true;_} | {scanner = None;_} ->
      Jr.Exception.raise_error Sxfiler_rpc.Errors.Scanner.already_exists
    | {scanner = Some s;_} -> Lwt.return s
end

module Get(Gateway: G.Scanner.Get) = struct
  type params = Gateway.params

  type result = T.Scanner.t

  let params_of_json = `Required Gateway.params_of_yojson
  let result_to_json = `Result T.Scanner.to_yojson

  let handle params =
    let%lwt result = Gateway.handle params in
    match result with
    | {Gateway.not_found = true;_} | {scanner = None;_} ->
      Jsonrpc_ocaml_yojson.(Exception.raise_error Sxfiler_rpc.Errors.Scanner.not_found)
    | {scanner = Some s; _} -> Lwt.return s
end

let expose server =
  let module S = Jsonrpc_ocaml_yojson.Server in
  let module W = Sxfiler_usecase.Scanner in
  let module I = Sxfiler_server_infra in

  let module Scanner_repo = I.Scanner_repo.Make(Global.Root) in
  let module Make_gateway = G.Scanner.Make(System.Real)(U.Scanner.Make(Scanner_repo)(I.Node_repo)) in
  let module Make = Procedure_intf.Make(Make(Make_gateway)) in

  let module Get_gateway = G.Scanner.Get(U.Scanner.Get(Scanner_repo)) in
  let module Get = Procedure_intf.Make(Get(Get_gateway)) in

  let module E = Sxfiler_rpc.Endpoints in
  List.fold_left (fun server (name, handler) ->
      S.expose ~_method:name ~handler server
    ) server [
    E.Scanner.Make.endpoint, Make.handler;
    E.Scanner.Get.endpoint, Get.handler;
  ]
