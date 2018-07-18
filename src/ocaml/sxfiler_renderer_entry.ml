open Sxfiler_renderer
module T = Sxfiler_types
module RI = Sxfiler_rpc
module R = Jsoo_reactjs
module C = Sxfiler_renderer_core
module B = Sxfiler_renderer_behavior
module S = Sxfiler_renderer_store

let container_id = "top-entry"
let target_port = 50879

let connect_ws url = new%js WebSockets.webSocket (Js.string url)

let notification_handler server message =
  let module R = Jsonrpc_ocaml_jsoo in
  let request = R.Request.of_json (Js._JSON##parse message##.data) in
  let thread = match request with
    | Ok req -> C.Rpc.Server.handle_request ~request:req server
    | Error _ -> Lwt.return_unit
  in
  Lwt.ignore_result thread

let () =
  let container = Dom_html.getElementById container_id in

  let websocket = connect_ws (Printf.sprintf "ws://localhost:%d" target_port) in
  let websocket_handler = Websocket_handler.make websocket in
  Websocket_handler.init websocket_handler;
  let rpc = Rpc.make websocket websocket_handler in
  let module L = Locator.Make((val rpc)) in
  let rpc_notification_server = C.Rpc.Server.make () in
  let module Ctx = (val Context.make rpc (module L)) in
  let rpc_notification_server = Notification_reducer.expose ~locator:(module L) rpc_notification_server in
  let store = Ctx.(Context.get_store instance) in
  Ctx.(Context.execute instance (module B.Initialize_stores) (module struct
         type message = C.Message.t
         module Store = S.App.Store
         let instance = store
       end));

  Websocket_handler.add websocket_handler ~handler:(notification_handler rpc_notification_server);

  websocket##.onopen := Dom.handler (fun _ ->
      (* Get current properties *)
      Ctx.(Context.execute Ctx.instance (module B.Refresh_keybindings) ()) |> Lwt.ignore_result;
      Ctx.(Context.execute Ctx.instance (module B.Refresh_configuration) ()) |> Lwt.ignore_result;

      List.iter (fun name ->
          let module R = Sxfiler_rpc in
          let param = Some {
              R.Scanner.Make_sync.initial_location = ".";
              name;
            } in
          C.Rpc.Client.request rpc (module C.Api.Scanner.Make_sync) param (function
              | Error e ->
                let module Ro = Jsonrpc_ocaml_jsoo in
                if e.Ro.Error.code = R.Errors.Scanner.already_exists then
                  Ctx.Context.execute Ctx.instance (module B.Refresh_scanner) name |> Lwt.ignore_result
                else ()
              | Ok _ -> ()
            )

          |> Lwt.ignore_result;
        ) [Const.scanner_1;Const.scanner_2];
      Js._true
    );

  let element = R.create_element ~props:(object%js
      val context = (module Ctx : Context.Instance)
    end) Components.main in
  R.dom##render element container
