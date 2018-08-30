open Sxfiler_renderer
module R = Jsoo_reactjs
module C = Sxfiler_renderer_core
module U = Sxfiler_renderer_usecase
module S = Sxfiler_renderer_store
module SI = Sxfiler_renderer_service_impl

let container_id = "top-entry"
let target_port = 50879
let connect_ws url = new%js WebSockets.webSocket (Js.string url)

let notification_handler server message =
  let module R = Jsonrpc_ocaml_jsoo in
  let request = R.Request.of_json (Js._JSON##parse message##.data) in
  let thread =
    match request with
    | Ok req -> C.Rpc.Server.handle_request ~request:req server
    | Error _ -> Lwt.return @@ Jsonrpc_ocaml_jsoo.Response.empty
  in
  Lwt.ignore_result thread

let expose_commands (module Locator : Locator.S) =
  let module Com = Sxfiler_renderer_command in
  Com.expose_static Locator.command_registry Locator.service_registry |> ignore ;
  Com.expose_dynamic Locator.dynamic_command_registry Locator.service_registry |> ignore

let () =
  Logs.set_reporter @@ Logs_browser.console_reporter () ;
  Logs.set_level (Some Logs.App) ;
  let container = Dom_html.getElementById container_id in
  let websocket = connect_ws (Printf.sprintf "ws://localhost:%d" target_port) in
  let websocket_handler = Websocket_handler.make websocket in
  Websocket_handler.init websocket_handler ;
  let rpc = Rpc.make websocket websocket_handler in
  let store = Locator.make_store () in
  let module Ctx = (val C.Context.make_instance (module Context) store) in
  let module Client = C.Rpc.Make_client ((val rpc)) in
  let module L =
    Locator.Make (Client) (Ctx)
      (struct
        let this = store
      end)
  in
  let module D = (val Ctx.(Context.dispatcher this)) in
  let rpc_notification_server = C.Rpc.Server.make () in
  let rpc_notification_server =
    Notification_reducer.expose
      ~dispatcher:(module D : C.Dispatcher.Instance)
      rpc_notification_server
  in
  Websocket_handler.add websocket_handler ~handler:(notification_handler rpc_notification_server) ;
  expose_commands (module L) ;
  websocket##.onopen :=
    Dom.handler (fun _ ->
        (* Get current properties *)
        let module Service = SI.Keymap.Make (Client) in
        let module I =
          ( val C.Usecase.make_instance
              (module U.Activate_mode.Make (Service))
              ~param:C.Types.Mode.File_tree )
        in
        Ctx.(Context.execute this (module I)) |> Lwt.ignore_result ;
        let module Service = SI.Configuration.Make (Client) in
        let module I =
          (val C.Usecase.make_instance (module U.Refresh_configuration.Make (Service)) ~param:())
        in
        Ctx.(Context.execute this (module I)) |> Lwt.ignore_result ;
        List.iter
          (fun pos ->
             let module Service = SI.Filer.Make (Client) in
             let module I =
               ( val C.Usecase.make_instance
                   (module U.Initialize_filer.Make (Service))
                   ~param:{initial_location = "."; pos} )
             in
             Ctx.(Context.execute this (module I)) |> Lwt.ignore_result )
          [`Left; `Right] ;
        Js._true ) ;
  let element =
    R.create_element
      ~props:
        (object%js
          val locator : (module Locator.S) = (module L)
        end)
      C_main.t
  in
  R.dom##render element container
