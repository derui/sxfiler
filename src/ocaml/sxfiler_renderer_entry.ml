open Sxfiler_renderer
module T = Sxfiler_types
module RI = Sxfiler_rpc
module R = Jsoo_reactjs

let container_id = "top-entry"
let target_port = 50879

let connect_ws url = new%js WebSockets.webSocket (Js.string url)

let notification_handler server message =
  let module R = Jsonrpc_ocaml_jsoo in
  let request = R.Request.of_json (Js._JSON##parse message##.data) in
  let thread = match request with
    | Ok req -> Rpc.Notification_server.handle_request ~request:req server
    | Error _ -> Lwt.return_unit
  in
  Lwt.ignore_result thread

let () =
  let container = Dom_html.getElementById container_id in

  let websocket = connect_ws (Printf.sprintf "ws://localhost:%d" target_port) in
  let websocket_handler = Websocket_handler.make websocket in
  Websocket_handler.init websocket_handler;
  let rpc_client = Rpc.Client.make websocket websocket_handler in
  let rpc_notification_server = Rpc.Notification_server.make () in
  let store = Store.make () in
  let rpc_notification_server = Notification_reducer.expose ~store rpc_notification_server in

  Websocket_handler.add websocket_handler ~handler:(notification_handler rpc_notification_server);

  let dispatcher = Dispatcher.make store rpc_client in
  websocket##.onopen := Dom.handler (fun _ ->
      List.iter (fun name ->
          let module R = Sxfiler_rpc in
          Rpc.Client.request rpc_client (module Api.Workspace.Make_sync) (fun res ->
              if res.R.Workspace.Make_sync.created then
                Rpc.Client.request rpc_client (module Api.Workspace.Get_sync) (fun res ->
                    Firebug.console##log (Js._JSON##stringify res);
                    let state = Store.get store in
                    Store.update store @@ State.with_stack state ~name ~f:(fun stack ->
                        let viewer = Types.Viewer.(File_tree {
                            File_tree.snapshot = res.T.Workspace.current;
                            selected_item_index = 0;
                          }) in
                        let stack = match stack with
                          | None -> Types.Viewer_stack.empty ()
                          | Some stack -> stack
                        in
                        Types.(Viewer_stack.push stack ~v:(Viewer_state.make viewer))
                      )
                  )
                  (Some {RI.Workspace.Get_sync.name = name})
                |> Lwt.ignore_result
              else
                ()
            )
            (Some {
                R.Workspace.Make_sync.initial_directory = ".";
                name;
              })
          |> Lwt.ignore_result;
        ) [Const.workspace_1;Const.workspace_2];
      Js._true
    );

  let element = R.create_element ~props:(object%js
      val dispatch = dispatcher
      val store = store
    end) Components.main in
  R.dom##render element container
