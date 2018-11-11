open Sxfiler_renderer
module R = Jsoo_reactjs
module C = Sxfiler_renderer_core
module U = Sxfiler_renderer_usecase
module S = Sxfiler_renderer_store
module SI = Sxfiler_renderer_service

let container_id = "top-entry"
let target_port = 50879
let connect_ws url = new%js WebSockets.webSocket (Js.string url)

let notification_handler server message =
  let module R = Jsonrpc_ocaml_jsoo in
  let request = R.Request.of_json (Js._JSON##parse message##.data) in
  let thread =
    match request with
    | Ok req -> C.Rpc_server.handle_request ~request:req server
    | Error _ -> Lwt.return @@ Jsonrpc_ocaml_jsoo.Response.empty
  in
  Lwt.ignore_result thread

let expose_commands (module Locator : Locator.S) =
  let module Com = Sxfiler_renderer_command in
  Com.expose_static Locator.command_registry Locator.service_registry |> ignore ;
  Com.expose_dynamic Locator.dynamic_command_registry Locator.service_registry |> ignore

let setup_planner (module Locator : Locator.S) =
  let module P = Sxfiler_renderer_background.Planner in
  let module C = (val Locator.context) in
  let module D = (val C.(Context.dispatcher this)) in
  let state = S.App.Store.get Locator.store in
  P.initialize state ;
  let open Sxfiler_core.Option in
  P.start () >|= fun (accepter, _) -> D.(Dispatcher.subscribe this ~f:accepter)

let setup_keymap_refresher (module Locator : Locator.S) =
  let module K = Sxfiler_renderer_background.Keymap in
  let module C = (val Locator.context) in
  let module D = (val C.(Context.dispatcher this)) in
  let state = S.App.Store.get Locator.store in
  K.initialize state ;
  let open Sxfiler_core.Option in
  K.start (module D) Locator.service_registry
  >|= fun (accepter, _) -> D.(Dispatcher.subscribe this ~f:accepter)

let () =
  Logs.set_reporter @@ Logs_browser.console_reporter () ;
  Logs.set_level (Some Logs.App) ;
  let websocket = connect_ws (Printf.sprintf "ws://localhost:%d" target_port) in
  let websocket_handler = Websocket_handler.make websocket in
  Websocket_handler.init websocket_handler ;
  let rpc = Rpc.make websocket websocket_handler in
  let store = Locator.make_store () in
  let module Jo = Jsonrpc_ocaml_jsoo in
  let module Ctx = (val C.Context.make_instance (module Context) store) in
  let module Client = Jo.Client.Make ((val rpc)) in
  let module L =
    Locator.Make (Client) (Ctx)
      (struct
        let this = store
      end)
  in
  let module D = (val Ctx.(Context.dispatcher this)) in
  let rpc_notification_server = C.Rpc_server.make () in
  let rpc_notification_server =
    Notification_reducer.expose ~context:(module Ctx) rpc_notification_server
  in
  Websocket_handler.add websocket_handler ~handler:(notification_handler rpc_notification_server) ;
  expose_commands (module L) ;
  (* TODO: should handle stopper *)
  setup_planner (module L) |> ignore ;
  setup_keymap_refresher (module L) |> ignore ;
  websocket##.onopen :=
    Dom.handler (fun _ ->
        (* Get current properties *)
        let module Service = SI.Configuration.Make (Client) in
        let instance =
          C.Usecase.make_instance (module U.Refresh_configuration.Make (Service)) ~param:()
        in
        Ctx.(Context.execute this instance) |> Lwt.ignore_result ;
        let open Lwt in
        ignore_result
          ( Lwt_js.yield ()
            >>= fun () ->
            let%lwt () =
              Lwt_list.iter_p
                (fun pos ->
                   let module Service = SI.Filer.Make (Client) in
                   let instance =
                     C.Usecase.make_instance
                       (module U.Initialize_filer.Make (Service))
                       ~param:{initial_location = "."; pos}
                   in
                   Ctx.(Context.execute this instance) )
                [`Left; `Right]
            in
            let i = C.Usecase.make_instance (module U.Finish_bootstrap) ~param:() in
            Ctx.(Context.execute this i) ) ;
        Js._true ) ;
  let element =
    R.create_element
      ~props:
        (object%js
          val locator : (module Locator.S) = (module L)
        end)
      C_main.t
  in
  let container = Dom_html.getElementById container_id in
  R.dom##render element container
