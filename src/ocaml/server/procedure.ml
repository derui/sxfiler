module G = Sxfiler_server_gateway
module Rpc = Jsonrpc_yojson
module Log = (val Sxfiler_server_core.Logger.make [ "rpc" ])

(** Types of spec of RPC to create procedure handler. This interface requires to define result and
    param always if procedure not required. *)

module type Spec = sig
  module Gateway : G.Core.Gateway

  val method_ : string
  val param_requirement : [ `Required | `Not_required of Gateway.input ]
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
      Log.info (fun m -> m "Start procedure: {%s}" method_);%lwt
      Log.debug (fun m ->
          m "Given parameters: %s"
            (Option.get ~default:(fun () -> `Null) req |> Yojson.Safe.to_string));%lwt
      let%lwt result =
        let execute_with_param decoder =
          match req with
          | None ->
              Logs.warn (fun m -> m "Required parameter not found");
              raise Rpc.(Error.Jsonrpc_error (Error.make Jsonrpc.Types.Error_code.Invalid_params))
          | Some params -> (
              match decoder params with
              | Error _ ->
                  Logs.warn (fun m ->
                      m "Required parameter can not encode: %s" (Yojson.Safe.to_string params));
                  raise
                    Rpc.(Error.Jsonrpc_error (Error.make Jsonrpc.Types.Error_code.Invalid_params))
              | Ok param -> S.Gateway.handle param )
        in
        match S.param_requirement with
        | `Not_required param -> S.Gateway.handle param
        | `Required -> execute_with_param S.Gateway.input_of_json
      in
      match result with
      | Error e -> Errors.of_gateway_error e
      | Ok output ->
          Log.info (fun m -> m "Finish procedure: {%s}" method_);%lwt
          S.Gateway.output_to_json output |> Option.some |> Lwt.return_ok
    with
    | Rpc.Error.Jsonrpc_error e as exn ->
        let%lwt () = Log.err (fun m -> m "Error occurred: %s" Rpc.Error.(to_string e)) in
        raise exn
    | _ as e ->
        let exn = Stdlib.Printexc.to_string e in
        let%lwt () = Log.err (fun m -> m "Not handled exception: %s" exn) in
        raise
          Rpc.(Error.Jsonrpc_error (Error.make (Jsonrpc.Types.Error_code.Server_error (-32000))))
end
