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
      Rpc.request rpc (module Api.Current_state) store (Some ()) |> Lwt.ignore_result;
      Js._true
    );

  let element = R.create_element ~props:(object%js
      val dispatch = dispatcher
      val store = store
    end) Components.main in
  R.dom##render element container
