(** Types of RPC to create procedure handler.
    This interface requires to define result and param always if procedure not required.
*)
module type Rpc_type = sig
  type params
  type result

  val params_of_json :
    [`Required of Yojson.Safe.json -> (params, string) Pervasives.result | `Not_required of params]
  (** [param_of_json f] convert json to [param] if it needed. Do not convert param if
      [`Not_required] specified.  *)

  val result_to_json : result -> Yojson.Safe.json
  (** [result_of_json f] convert [result] to json if it needed. Do not return anything if
      [`Void] specified.  *)

  val handle : params -> result Lwt.t
  (** Define handling with [param] and return [result]. *)
end

module type S = sig
  type params
  type result

  val handler : Jsonrpc_ocaml_yojson.Request.t -> Jsonrpc_ocaml_yojson.Response.t Lwt.t
  (** Handle around RPC without boring conversions for request and response. *)
end

(** Make a module to be able to handle request and response of JSON-RPC without boilarplate. *)
module Make (R : Rpc_type) : S with type params := R.params and type result := R.result = struct
  let handler req =
    let module Rpc = Jsonrpc_ocaml_yojson in
    let module Req = Rpc.Request in
    let module Res = Rpc.Response in
    let open Sxfiler_core in
    let tags = Sxfiler_server_core.Logger.Tags.module_lib ["rpc"] in
    try%lwt
      Logs_lwt.debug (fun m ->
          m ~tags "Start procedure: {%s}, id: {%Ld}" req.Req._method
            (Option.get ~default:(fun () -> 0L) req.Req.id) ) ;%lwt
      let%lwt result =
        let execute_with_param decoder =
          match req.Req.params with
          | None ->
            Logs.warn (fun m -> m ~tags "Required parameter not found") ;
            Rpc.(Exception.raise_error Types.Error_code.Invalid_params)
          | Some params -> (
              match decoder params with
              | Error _ ->
                Logs.warn (fun m ->
                    m ~tags "Required parameter can not encode: %s"
                      (Yojson.Safe.to_string params) ) ;
                Rpc.(Exception.raise_error Types.Error_code.Invalid_params)
              | Ok param -> R.handle param )
        in
        match R.params_of_json with
        | `Not_required param -> R.handle param
        | `Required f -> execute_with_param f
      in
      Logs_lwt.info (fun m ->
          m ~tags "Finish procedure: {%s}, id: {%Ld}" req._method
            (Option.get ~default:Fun.(const 0L) req.id) ) ;%lwt
      let result = R.result_to_json result |> Option.some in
      Lwt.return {Res.result; id = req.Req.id; error = None}
    with Rpc.Exception.Jsonrpc_error (code, _) as e ->
      Logs_lwt.err (fun m -> m ~tags "Error occurred: %s" (Rpc.Types.Error_code.to_message code)) ;%lwt
      raise e
end
