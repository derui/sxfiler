(** Types of RPC to create procedure handler.
    This interface requires to define result and param always if procedure not required.
 *)
module type Rpc_type = sig
  type param
  type result

  (** [param_of_json f] convert json to [param] if it needed. Do not convert param if
      [`Not_required] specified.  *)
  val param_of_json : [`Required of Yojson.Safe.json -> (param, string) Pervasives.result | `Not_required of param]

  (** [result_of_json f] convert [result] to json if it needed. Do not return anything if
      [`Void] specified.  *)
  val result_to_json: [`Void | `Result of result -> Yojson.Safe.json]

  (** Define handling with [param] and return [result]. *)
  val handle : param -> result Lwt.t
end

module type S = sig
  type param
  type result

  (** Handle around RPC without boring conversions for request and response. *)
  val handler: Jsonrpc_ocaml_yojson.Request.t -> Jsonrpc_ocaml_yojson.Response.t Lwt.t
end

module Make(R:Rpc_type) : S with type param := R.param and type result := R.result = struct

  let handler req =
    let module Rpc = Jsonrpc_ocaml_yojson in
    let module Req = Rpc.Request in
    let module Res = Rpc.Response in
    let%lwt result = match R.param_of_json with
      | `Not_required param -> R.handle param
      | `Required f -> begin match req.Req.params with
          | None -> raise Rpc.Types.(Jsonrpc_error Error_code.Invalid_params)
          | Some params -> begin match f params with
              | Error _ -> raise Rpc.Types.(Jsonrpc_error Error_code.Invalid_params)
              | Ok param -> R.handle param
            end
        end
    in
    let result = match R.result_to_json with
      | `Void -> None
      | `Result f -> Some (f result)
    in
    Lwt.return Res.({
        result;
        id = req.Req.id;
        error = None
      })
end
