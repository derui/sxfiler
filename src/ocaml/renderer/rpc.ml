(* Provides RPC interface via WebSocket *)
open Sxfiler_core
module Rpc = Jsonrpc_ocaml.Rpc
module R = Jsonrpc_ocaml_jsoo

module type Rpc = Sxfiler_renderer_core.Rpc.S

type request_task =
  { waiter : R.Response.t Lwt.t
  ; wakener : R.Response.t Lwt.u }

type t =
  { ws : WebSockets.webSocket Js.t
  ; (* id_handler_map is mutable object. *)
    id_task_map : request_task Jstable.t }

let make ws handlers =
  let id_task_map = Jstable.create () in
  (* apply message handler to given websocket *)
  let message_handler message =
    let open Option in
    let response = R.Response.of_json (Js._JSON##parse message##.data) in
    match response with
    | Ok res ->
      res.R.Response.id
      >>= (fun id ->
          let key = Js.string @@ Int64.to_string id in
          Jstable.find id_task_map key |> Js.Optdef.to_option
          >|= fun task -> Lwt.wakeup_later task.wakener res ; Jstable.remove id_task_map key )
      |> ignore
    | Error _ -> (* FIXME: should handle error *)
      ()
  in
  Websocket_handler.add handlers ~handler:message_handler ;
  let t = {ws; id_task_map} in
  ( module struct
    type json = < > Js.t

    module Thread = Lwt
    module Request = R.Request
    module Response = R.Response

    let call req =
      let waiter =
        match req.R.Request.id with
        | Some id ->
          let waiter, wakener = Lwt.task () in
          Jstable.add t.id_task_map (Js.string @@ Int64.to_string id) {waiter; wakener} ;
          waiter
        | None -> raise Error.(create "API requires ID" |> to_exn)
      in
      let json = Js._JSON##stringify (R.Request.to_json req) in
      (t.ws)##send json ;
      waiter

    let notify req =
      let json = Js._JSON##stringify (R.Request.to_json req) in
      (t.ws)##send json ;
      Lwt.return ()
  end
  : Rpc )
