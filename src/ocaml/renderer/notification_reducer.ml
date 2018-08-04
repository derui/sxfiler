(** {!Notification_reducer} defines handlers for notification sent from server.  *)

module T = Sxfiler_domain
module R = Sxfiler_rpc
module Rj = Sxfiler_rpc_jsoo
module Jr = Jsonrpc_ocaml_jsoo
module C = Sxfiler_renderer_core

module type Dispatcher = C.Dispatcher_intf.Instance

module Scanner_update(Dispatcher:Dispatcher) = struct
  include R.Notification.Scanner_update
  open Rj.Notification.Scanner_update

  let handler req =
    match req.Jr.Request.params with
    | None -> Lwt.return Jr.Response.empty
    | Some params ->
      let params = params_of_json @@ Js.Unsafe.coerce params in
      let open Lwt.Infix in

      Lwt.return @@ Dispatcher.(Dispatcher.dispatch this @@ C.Message.Update_scanner params.scanner) >>= fun () ->
      Lwt.return Jr.Response.empty
end

let expose ~dispatcher server =
  let module Scanner_update = Scanner_update((val dispatcher : Dispatcher)) in
  C.Rpc.Server.expose ~_method:Scanner_update.name ~handler:Scanner_update.handler server
