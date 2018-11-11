module type Api_def = Jsonrpc_ocaml_jsoo.Client.Api_def

module R = Jsonrpc_ocaml_jsoo
module C = Sxfiler_renderer_core

module type Gen_res = sig
  val gen : int64 option -> R.Response.t
end

module Dummy_rpc (Res : Gen_res) = struct
  type json = < > Js.t

  module Thread = Lwt
  module Request = R.Request
  module Response = R.Response

  (* Call api as request with definition and parameter *)
  let call req =
    let res = Res.gen req.R.Request.id in
    Lwt.return res

  (* Call api as notification with definition and parameter *)
  let notify (_ : R.Request.t) = Lwt.return_unit
end

module Make_client (Res : Gen_res) = R.Client.Make (Dummy_rpc (Res))
