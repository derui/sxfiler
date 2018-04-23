open Sxfiler_renderer
module C = Sxfiler_common
module R = Jsoo_reactjs

let container_id = "top-entry"
let target_port = 50879

module Publisher = struct
  type subscriber = C.State.t -> unit
  type t = {
    mutable subscribers: subscriber list
  }

  let make () = {
    subscribers = []
  }

  let subscribe subscriber t = t.subscribers <- subscriber :: t.subscribers

  let publish t v = List.iter (fun f -> f v) t.subscribers
end

let connect_ws url = new%js WebSockets.webSocket (Js.string url)

let () =
  let electron = Modules.electron in
  let container = Dom_html.getElementById container_id in
  let dispatcher = Key_dispatcher.make electron##.ipcRenderer in
  let publisher = Publisher.make () in

  let websocket = connect_ws (Printf.sprintf "ws://localhost:%d" target_port) in

  let handle _ = function
    | C.Event.IPC.Update t -> C.State.of_js t |> Publisher.publish publisher
    | _ -> ()
  in

  C.Event.IPC.(on ~target:Listener.update ~f:handle electron##.ipcRenderer);

  let element = R.create_element ~props:(object%js
      val dispatch = dispatcher
      val state = C.State.empty
      method subscribe = (fun f -> Publisher.subscribe f publisher)
    end) Components.main in
  R.dom##render element container
