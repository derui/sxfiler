module Log = (val Sxfiler_server_core.Logger.make ["rpc"])

(** Types of spec of RPC to create procedure handler.
    This interface requires to define result and param always if procedure not required.
*)
module Spec = struct
  type 'params param_conv =
    [ `Required of Yojson.Safe.t -> ('params, string) Pervasives.result
    | `Not_required of 'params ]

  type 'result result_conv = 'result -> Yojson.Safe.t

  type ('params, 'result) t =
    { params_of_json : 'params param_conv
    ; result_to_json : 'result result_conv
    ; handle : 'params -> 'result Lwt.t }
end

type procedure =
  { method_ : string
  ; handler : Jsonrpc_ocaml_yojson.Request.t -> Jsonrpc_ocaml_yojson.Response.t Lwt.t }

let to_procedure (type p r) ~method_ ~(spec : (p, r) Spec.t) =
  let handler req =
    let module Rpc = Jsonrpc_ocaml_yojson in
    let module Req = Rpc.Request in
    let module Res = Rpc.Response in
    let open Sxfiler_core in
    try%lwt
      Log.debug (fun m ->
          m "Start procedure: {%s}, id: {%Ld}" req.Req._method
            (Option.get ~default:(fun () -> 0L) req.Req.id) ) ;%lwt
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
              | Ok param -> spec.handle param )
        in
        match spec.params_of_json with
        | `Not_required param -> spec.handle param
        | `Required f -> execute_with_param f
      in
      Log.info (fun m ->
          m "Finish procedure: {%s}, id: {%Ld}" req._method
            (Option.get ~default:Fun.(const 0L) req.id) ) ;%lwt
      let result = spec.result_to_json result |> Option.some in
      Lwt.return {Res.result; id = req.Req.id; error = None}
    with Rpc.Exception.Jsonrpc_error (code, _) as e ->
      Log.err (fun m -> m "Error occurred: %s" (Rpc.Types.Error_code.to_message code)) ;%lwt
      raise e
  in
  {method_; handler}