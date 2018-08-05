open Sxfiler_renderer
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
    | Error _ -> Lwt.return @@ Jsonrpc_ocaml_jsoo.Response.empty
  in
  Lwt.ignore_result thread

let expose_commands (module Locator: Locator.Main) =
  let module Com = Sxfiler_renderer_command in
  Com.expose_static Locator.command_registry |> ignore

let () =
  Logs.set_reporter @@ Logs_browser.console_reporter ();
  Logs.set_level (Some Logs.App);
  let container = Dom_html.getElementById container_id in

  let websocket = connect_ws (Printf.sprintf "ws://localhost:%d" target_port) in
  let websocket_handler = Websocket_handler.make websocket in
  Websocket_handler.init websocket_handler;
  let rpc = Rpc.make websocket websocket_handler in
  let store = Locator.make_store () in
  let module Ctx = (val C.Context.make_instance (module Context) store) in
  let module L = Locator.Make((val rpc))(Ctx)(struct
      let instance = store
    end) in
  let module D = (val Ctx.(Context.dispatcher this)) in
  let rpc_notification_server = C.Rpc.Server.make () in
  let rpc_notification_server = Notification_reducer.expose ~dispatcher:(module D) rpc_notification_server in

  Websocket_handler.add websocket_handler ~handler:(notification_handler rpc_notification_server);

  expose_commands (module L);

  websocket##.onopen := Dom.handler (fun _ ->
      (* Get current properties *)
      let module I = (val C.Behavior.make_instance (module B.Refresh_keymap) ~config:(module L) ~param:()) in
      Ctx.(Context.execute this (module I)) |> Lwt.ignore_result;
      let module I = (val C.Behavior.make_instance (module B.Refresh_configuration) ~config:(module L) ~param:()) in
      Ctx.(Context.execute this (module I)) |> Lwt.ignore_result;

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
                  let module I = (val C.Behavior.make_instance (module B.Refresh_scanner) ~config:(module L) ~param:name) in
                  Ctx.(Context.execute this (module I)) |> ignore
                else ()
              | Ok _ -> ()
            )

          |> Lwt.ignore_result;
        ) [Const.scanner_1;Const.scanner_2];
      Js._true
    );

  let element = R.create_element ~props:(object%js
      val locator = (module L : Locator.Main)
    end) C_main.t in
  R.dom##render element container
