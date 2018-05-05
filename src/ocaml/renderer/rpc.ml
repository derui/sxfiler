(* Provides RPC interface via WebSocket *)

module R = Jsonrpc_ocaml

type t = {
  ws: WebSockets.webSocket Js.t;
  message_handler: (WebSockets.webSocket Js.t, WebSockets.webSocket WebSockets.messageEvent Js.t) Dom.event_listener;
  (* id_handler_map is mutable object. *)
  id_handler_map: (R.Response.t -> unit) Jstable.t
}

module type Rpc = R.Client.Rpc with module Thread := Lwt

let make ws =
  let id_handler_map = Jstable.create () in
  (* apply message handler to given websocket *)
  let message_handler = Dom.handler (fun message ->
      let open Minimal_monadic_caml.Option.Infix in
      let response = R.Response.of_json (Yojson.Basic.from_string @@ Js.to_string message##.data) in
      let _ = match response with
        | Ok res -> begin
            res.R.Response.id >>= (fun id ->
                let key = Js.string @@ Int64.to_string id in
                Jstable.find id_handler_map key |> Js.Optdef.to_option >>= fun handler ->
                handler res;
                Jstable.remove id_handler_map key;

                Some ()
              ) |> ignore;
          end
        | Error _ ->
          (* FIXME: should handle error *)
          ()
      in
      Js._true
    )
  in
  ws##.onmessage := message_handler;
  let t = {ws; message_handler; id_handler_map} in

  (module struct
    module Thread = Lwt

    let call_api ?handler req =
      let _ = match (req.R.Request.id, handler) with
        | (Some id, Some handler) -> Jstable.add t.id_handler_map (Js.string @@ Int64.to_string id) handler
        | (None, _) | (_, None) -> ()
      in

      let json = R.Request.to_json req |> Yojson.Basic.to_string |> Js.string in
      t.ws##send json;
      Thread.return ()

  end : Rpc)

(* Call api as request with definition and parameter *)
let request
    (type p)
    (type r)
    (module Rpc: Rpc)
    (module Api: Api.Api_def with type params = p and type result = r)
    (handler: r -> unit)
    (param: p option)
  =
  let module C = R.Client in
  let req, handler = C.make_request (module Api) param (function
      | Ok r -> handler r
      | Error _ -> ())
  in

  Rpc.call_api ?handler req

(* Call api as notification with definition and parameter *)
let notification (type p)
    (module Rpc: Rpc)
    (module Api: Api.Api_def with type params = p)
    (param: p option)
  =
  let module C = R.Client in
  let req, handler = C.make_notification (module Api) param in
  Rpc.call_api ?handler req
