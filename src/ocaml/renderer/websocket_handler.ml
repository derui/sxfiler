(** Providing functions that handle message of Websocket.   *)
type t = {
  ws: WebSockets.webSocket Js.t;
  mutable handlers: (WebSockets.webSocket WebSockets.messageEvent Js.t -> unit) list;
}

let make ws = {
  ws;
  handlers = [];
}

let init t =
  t.ws##.onmessage := Dom.handler (fun message ->
      List.iter (fun handler -> handler message) t.handlers;
      Js._true)

let add t ~handler = t.handlers <- handler :: t.handlers
