(* Provides RPC interface via WebSocket *)
open Sxfiler_core
module Rpc = Jsonrpc_ocaml.Rpc_intf
module R = Jsonrpc_ocaml_jsoo

module type Rpc =
  Rpc.S with module Thread := Lwt and module Response := R.Response and module Request := R.Request

type t =
  { ws : WebSockets.webSocket Js.t
  ; (* id_handler_map is mutable object. *)
    id_handler_map : (R.Response.t -> unit) Jstable.t }

let make ws handlers =
  let id_handler_map = Jstable.create () in
  (* apply message handler to given websocket *)
  let message_handler message =
    let open Option in
    let response = R.Response.of_json (Js._JSON##parse message##.data) in
    match response with
    | Ok res ->
      res.R.Response.id
      >>= (fun id ->
          let key = Js.string @@ Int64.to_string id in
          Jstable.find id_handler_map key |> Js.Optdef.to_option
          >>= fun handler -> handler res ; Jstable.remove id_handler_map key ; Some () )
      |> ignore
    | Error _ -> (* FIXME: should handle error *)
      ()
  in
  Websocket_handler.add handlers ~handler:message_handler ;
  let t = {ws; id_handler_map} in
  ( module struct
    let call_api ?handler req =
      let _ =
        match (req.R.Request.id, handler) with
        | Some id, Some handler ->
          Jstable.add t.id_handler_map (Js.string @@ Int64.to_string id) handler
        | None, _ | _, None -> ()
      in
      let json = Js._JSON##stringify (R.Request.to_json req) in
      (t.ws)##send json ;
      Lwt.return ()
  end
  : Rpc )
