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
  val handle : Yojson.Safe.t option -> (Yojson.Safe.t option, Rpc.Error.t) result Lwt.t
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
      let handle param =
        match%lwt S.Gateway.handle param with
        | Error e -> Errors.of_gateway_error e |> Lwt.return_error
        | Ok output ->
            Log.info (fun m -> m "Finish procedure: {%s}" method_);%lwt
            S.Gateway.output_to_json output |> Option.some |> Lwt.return_ok
      in
      let decode_params decoder =
        let open Result.Infix in
        let* param =
          match req with
          | None ->
              Logs.warn (fun m -> m "Required parameter not found");
              Error Rpc.(Error.make Jsonrpc.Types.Error_code.Invalid_params)
          | Some params -> Ok params
        in
        (* all messages are protobuf. Protobuf in OCaml is pure string, so get directly it *)
        match decoder param with
        | Error e ->
            Logs.warn (fun m ->
                m "Required parameter not found %s" @@ Protocol_conv_json.Json.error_to_string_hum e);

            Error Rpc.(Error.make Jsonrpc.Types.Error_code.Invalid_params)
        | Ok param -> Ok param
      in
      match S.param_requirement with
      | `Not_required param -> handle param
      | `Required -> (
          match decode_params S.Gateway.input_of_json with
          | Error e -> Lwt.return_error e
          | Ok param -> handle param )
    with _ as e ->
      let exn = Stdlib.Printexc.to_string e in
      let%lwt () = Log.err (fun m -> m "Not handled exception: %s" exn) in
      Lwt.return_error Rpc.(Error.make (Jsonrpc.Types.Error_code.Server_error (-32000)))
end
