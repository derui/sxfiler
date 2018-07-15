(** {!Notification_reducer} defines handlers for notification sent from server.  *)

module T = Sxfiler_types
module R = Sxfiler_rpc
module Rj = Sxfiler_rpc_jsoo
module Jr = Jsonrpc_ocaml_jsoo
module C = Sxfiler_renderer_core

module Scanner_update(Repo:C.Repository_intf.Scanner_instance) = struct
  include R.Notification.Scanner_update
  open Rj.Notification.Scanner_update

  let handler req =
    match req.Jr.Request.params with
    | None -> Lwt.return_unit
    | Some params ->
      let params = params_of_json @@ Js.Unsafe.coerce params in
      let open Repo in
      Lwt.return @@ Repo.store instance params.scanner
end

let expose server =
  let module Scanner_update = Scanner_update(Locator.Repository.Scanner) in
  Rpc.Notification_server.expose ~_method:Scanner_update.name ~handler:Scanner_update.handler server
