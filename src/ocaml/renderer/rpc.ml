(* Provides RPC interface via WebSocket *)
open Sxfiler_core
module Rpc = Jsonrpc_ocaml.Rpc_intf
module R = Jsonrpc_ocaml_jsoo

module type Rpc = Rpc.S with module Thread := Lwt
                         and module Response := R.Response
                         and module Request := R.Request

module Client = struct

  type t = {
    ws: WebSockets.webSocket Js.t;
    (* id_handler_map is mutable object. *)
    id_handler_map: (R.Response.t -> unit) Jstable.t
  }

  let make ws handlers =
    let id_handler_map = Jstable.create () in
    (* apply message handler to given websocket *)
    let message_handler message =
      let open Option.Infix in
      let response = R.Response.of_json (Js._JSON##parse message##.data) in
      match response with
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
    Websocket_handler.add handlers ~handler:message_handler;
    let t = {ws; id_handler_map} in

    (module struct
      let call_api ?handler req =
        let _ = match (req.R.Request.id, handler) with
          | (Some id, Some handler) -> Jstable.add t.id_handler_map (Js.string @@ Int64.to_string id) handler
          | (None, _) | (_, None) -> ()
        in

        let json = Js._JSON##stringify (R.Request.to_json req) in
        t.ws##send json;
        Lwt.return ()

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

end

(** {!Notification_server} provides some functions to handle notification from server. *)
module Notification_server = struct
  type json = < > Js.t
  module Response = R.Response
  module Request = R.Request
  module Thread = Lwt
  type handler = R.Request.t -> unit Lwt.t
  type _method = string
  type t = {
    procedure_table: handler Jstable.t
  }

  let make () = {
    procedure_table = Jstable.create ();
  }

  let expose ~_method ~handler t =
    let _method' = Js.string _method in
    Jstable.add t.procedure_table _method' handler;
    t

  let unexpose ~_method t =
    let open Util.Optdef in
    Jstable.remove t.procedure_table Js.(string _method);
    t

  let handle_request ~request t =
    Firebug.console##log (Js._JSON##stringify request);
    let open Jsonrpc_ocaml_jsoo in
    match Jstable.find t.procedure_table Js.(string request.R.Request._method) |> Js.Optdef.to_option with
    | None ->
      (* TODO: logging unknown notification *)
      Lwt.return_unit
    | Some handler -> Lwt.catch
                        (fun () -> handler request)
                        (fun e -> raise e)

end
