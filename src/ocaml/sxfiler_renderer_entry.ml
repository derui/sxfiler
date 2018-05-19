open Sxfiler_renderer
module C = Sxfiler_common
module R = Jsoo_reactjs

let container_id = "top-entry"
let target_port = 50879

let connect_ws url = new%js WebSockets.webSocket (Js.string url)

let () =
  let container = Dom_html.getElementById container_id in

  let websocket = connect_ws (Printf.sprintf "ws://localhost:%d" target_port) in
  let rpc = Rpc.make websocket in
  let store = Store.make () in

  let dispatcher = Dispatcher.make store rpc in
  websocket##.onopen := Dom.handler (fun _ ->
      Rpc.request rpc (module Api.Root.Get_current_state) (fun server_state ->
          let root = store.Store.state in
          Store.update store {root with C.State.server_state}
        ) (Some ()) |> Lwt.ignore_result;

      Rpc.request rpc (module Api.Config.Load) (fun config ->
          let root = store.Store.state in
          Store.update store {root with C.State.config}
        ) (Some ()) |> Lwt.ignore_result;
      Js._true
    );

  let element = R.create_element ~props:(object%js
      val dispatch = dispatcher
      val store = store
    end) Components.main in
  R.dom##render element container
