module G = Sxfiler_server_gateway
module Rpc = Jsonrpc_yojson
module Log = (val Sxfiler_server_core.Logger.make ["rpc"])

(** Types of spec of RPC to create procedure handler. This interface requires to define result and
    param always if procedure not required. *)

module type Spec = sig
  module Gateway : G.Core.Gateway

  val method_ : string
  val param_requirement : [`Required | `Not_required of Gateway.params]
end

module type S = sig
  val method_ : string
  val handle : Rpc.Server.handler
end

module Make (S : Spec) : S = struct
  let method_ = S.method_

  let handle req =
    let module Req = Rpc.Request in
    let module Res = Rpc.Response in
    let open Sxfiler_core in
    try%lwt
      Log.info (fun m ->
          m "Start procedure: {%s}, param: %s" method_
            (Option.get ~default:(fun () -> `Null) req |> Yojson.Safe.to_string) ) ;%lwt
      let%lwt result =
        let execute_with_param decoder =
          match req with
          | None ->
            Logs.warn (fun m -> m "Required parameter not found") ;
            Rpc.(Exception.raise_error Jsonrpc.Types.Error_code.Invalid_params)
          | Some params -> (
              match decoder params with
              | Error _ ->
                Logs.warn (fun m ->
                    m "Required parameter can not encode: %s" (Yojson.Safe.to_string params) ) ;
                Rpc.(Exception.raise_error Jsonrpc.Types.Error_code.Invalid_params)
              | Ok param -> S.Gateway.handle param )
        in
        match S.param_requirement with
        | `Not_required param -> S.Gateway.handle param
        | `Required -> execute_with_param S.Gateway.params_of_json
      in
      Log.info (fun m -> m "Finish procedure: {%s}" method_) ;%lwt
      S.Gateway.result_to_json result |> Option.some |> Lwt.return_ok
    with
    | G.Gateway_error.Gateway_error e -> Errors.of_gateway_error e
    | _ as e ->
      let exn = Stdlib.Printexc.to_string e in
      let%lwt () = Log.err (fun m -> m "Error occurred: %s" exn) in
      Rpc.(Exception.raise_error (Jsonrpc.Types.Error_code.Server_error (-32000)))
end
