(** Types of RPC to create procedure handler.
    This interface requires to define result and param always if procedure not required.
*)
module type Rpc_type = sig
  type params
  type result

  (** [param_of_json f] convert json to [param] if it needed. Do not convert param if
      [`Not_required] specified.  *)
  val params_of_json : [ `Required of Yojson.Safe.json -> (params, string) Pervasives.result
                       | `Not_required of params]

  (** [result_of_json f] convert [result] to json if it needed. Do not return anything if
      [`Void] specified.  *)
  val result_to_json: result -> Yojson.Safe.json

  (** Define handling with [param] and return [result]. *)
  val handle : params -> result Lwt.t
end

module type S = sig
  type params
  type result

  (** Handle around RPC without boring conversions for request and response. *)
  val handler: Jsonrpc_ocaml_yojson.Request.t -> Jsonrpc_ocaml_yojson.Response.t Lwt.t
end

(** Make a module to be able to handle request and response of JSON-RPC without boilarplate. *)
module Make(R:Rpc_type) : S with type params := R.params and type result := R.result = struct

  let handler req =
    let module Rpc = Jsonrpc_ocaml_yojson in
    let module Req = Rpc.Request in
    let module Res = Rpc.Response in
    let open Sxfiler_core in
    let tags = Logger.Tags.module_lib ["rpc"] in

    let%lwt () = Logs_lwt.info @@ fun m -> m ~tags "Start procedure: {%s}, id: {%Ld}" req.Req._method (Option.get ~default:(fun () -> 0L) req.Req.id) in
    let%lwt result = match R.params_of_json with
      | `Not_required param -> R.handle param
      | `Required f -> begin match req.Req.params with
          | None -> Rpc.(Exception.raise_error Types.Error_code.Invalid_params)
          | Some params -> begin match f params with
              | Error _ -> Rpc.(Exception.raise_error Types.Error_code.Invalid_params)
              | Ok param -> R.handle param
            end
        end
    in
    let result = Option.some @@ R.result_to_json result in
    let%lwt () = Logs_lwt.info @@ fun m -> m ~tags "Finish procedure: {%s}, id: {%Ld}" req.Req._method (Option.get ~default:(fun () -> 0L) req.Req.id) in
    Lwt.return {
      Res.result;
      id = req.Req.id;
      error = None
    }
end
