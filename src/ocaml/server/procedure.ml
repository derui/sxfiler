module G = Sxfiler_server_gateway
module Rpc = Jsonrpc_ocaml_yojson
module Log = (val Sxfiler_server_core.Logger.make ["rpc"])

(** Types of spec of RPC to create procedure handler.
    This interface requires to define result and param always if procedure not required.
*)

module type Spec = sig
  module Gateway : G.Core.Gateway

  val method_ : string
  val param_requirement : [`Required | `Not_required of Gateway.params]
end

module type S = sig
  val method_ : string
  val handle : Jsonrpc_ocaml_yojson.Request.t -> Jsonrpc_ocaml_yojson.Response.t Lwt.t
end

let handle_error = function
  | G.Gateway_error.Unknown_error v -> Rpc.Exception.raise_error (Errors.unknown_error v)
  | Filer_already_exists -> Rpc.Exception.raise_error Errors.filer_already_exists
  | Filer_not_found -> Rpc.Exception.raise_error Errors.filer_not_found
  | Filer_not_directory -> Rpc.Exception.raise_error Errors.filer_not_directory
  | Node_not_found -> Rpc.Exception.raise_error Errors.node_not_found
  | Plan_not_found -> Rpc.Exception.raise_error Errors.plan_not_found
  | Plan_same_filer -> Rpc.Exception.raise_error Errors.plan_same_filer

module Make (S : Spec) : S = struct
  let method_ = S.method_

  let handle req =
    let module Req = Rpc.Request in
    let module Res = Rpc.Response in
    let open Sxfiler_core in
    try%lwt
      Log.info (fun m ->
          m "Start procedure: {%s}, id: {%Ld}, param: %s" req.Req._method
            (Option.get ~default:(fun () -> 0L) req.Req.id)
            (Option.get ~default:(fun () -> `Null) req.params |> Yojson.Safe.to_string) ) ;%lwt
      let%lwt result =
        let execute_with_param decoder =
          match req.Req.params with
          | None ->
            Logs.warn (fun m -> m "Required parameter not found") ;
            Rpc.(Exception.raise_error Types.Error_code.Invalid_params)
          | Some params -> (
              match decoder params with
              | Error _ ->
                Logs.warn (fun m ->
                    m "Required parameter can not encode: %s" (Yojson.Safe.to_string params) ) ;
                Rpc.(Exception.raise_error Types.Error_code.Invalid_params)
              | Ok param -> S.Gateway.handle param )
        in
        match S.param_requirement with
        | `Not_required param -> S.Gateway.handle param
        | `Required -> execute_with_param S.Gateway.params_of_json
      in
      Log.info (fun m ->
          m "Finish procedure: {%s}, id: {%Ld}" req._method
            (Option.get ~default:Fun.(const 0L) req.id) ) ;%lwt
      let result = S.Gateway.result_to_json result |> Option.some in
      Lwt.return {Res.result; id = req.Req.id; error = None}
    with
    | G.Gateway_error.Gateway_error e ->
      let%lwt () = Log.err (fun m -> m "Error from gateway error") in
      handle_error e
    | _ as e ->
      let exn = Stdlib.Printexc.to_string e in
      let%lwt () = Log.err (fun m -> m "Error occurred: %s" exn) in
      Rpc.(Exception.raise_error (Types.Error_code.Server_error (-32000)))
end
