module type Api_def = Jsonrpc_ocaml_jsoo.Client.Api_def

module R = Jsonrpc_ocaml_jsoo
module C = Sxfiler_renderer_core

module type Gen_res = sig
  val gen : int64 option -> R.Response.t
end

module Make_client (Res : Gen_res) : C.Rpc.Client = struct
  (* Call api as request with definition and parameter *)
  let request (type p r) (module Api : Api_def with type params = p and type result = r)
      (param : p option) (handler : (r option, R.Error.t) result -> unit) =
    let module C = R.Client in
    let req, handler = C.make_request (module Api) param handler in
    Lwt.return (match handler with None -> () | Some f -> Res.gen req.R.Request.id |> f)

  (* Call api as notification with definition and parameter *)
  let notification (type p) (module Api : Api_def with type params = p) (param : p option) =
    let module C = R.Client in
    let _, _ = C.make_notification (module Api) param in
    Lwt.return_unit
end
