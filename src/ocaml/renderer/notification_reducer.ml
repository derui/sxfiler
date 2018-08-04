(** {!Notification_reducer} defines handlers for notification sent from server.  *)

module T = Sxfiler_domain
module R = Sxfiler_rpc
module Jr = Jsonrpc_ocaml_jsoo
module C = Sxfiler_renderer_core
module P = Sxfiler_renderer_presenter

module type Dispatcher = C.Dispatcher_intf.Instance

module Scanner_update(Dispatcher:Dispatcher) = struct
  include R.Notification.Scanner_update

  class type js_params = object
    method name: Js.js_string Js.t Js.readonly_prop
    method scanner: P.Scanner.js Js.t Js.readonly_prop
  end

  let params_of_json js = {
    name = Js.to_string js##.name;
    scanner = P.Scanner.of_js js##.scanner;
  }

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
