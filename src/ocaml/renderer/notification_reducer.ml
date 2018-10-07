(** {!Notification_reducer} defines handlers for notification sent from server.  *)

module R = Sxfiler_rpc
module C = Sxfiler_renderer_core
module Jr = Jsonrpc_ocaml_jsoo
module U = Sxfiler_renderer_usecase
module T = Sxfiler_renderer_translator

module Sleeper = struct
  let sleep = Lwt_js.sleep
end

let message_notification_handler (module Ctx : Context.Instance) req =
  let open Sxfiler_core.Option in
  let open Sxfiler_core.Fun in
  let handling =
    req.Jr.Request.params
    >>= Js.Unsafe.coerce %> T.Notification.of_js %> some
    >>= fun param ->
    let usecase = C.Usecase.make_instance (module U.Notify_message.Make (Sleeper)) ~param in
    Some Lwt.(Ctx.(Context.execute this usecase) >>= fun () -> Lwt.return Jr.Response.empty)
  in
  get ~default:(fun () -> Lwt.return Jr.Response.empty) handling

let expose ~context server =
  let server =
    C.Rpc.Server.expose ~_method:R.Endpoints.Notification.Notify.endpoint
      ~handler:(message_notification_handler context)
      server
  in
  server
