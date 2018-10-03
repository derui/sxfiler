(** Proc_notification module defines functions for RPC of completion. *)
module SC = Sxfiler_server_core

module Jr = Jsonrpc_ocaml_yojson
module T = Sxfiler_domain
module U = Sxfiler_usecase
module G = Sxfiler_server_gateway

module Notify_message (G : G.Notification.Notify_message.S) = struct
  type params = G.params
  type result = unit

  let params_of_json = `Required G.params_of_yojson
  let result_to_json _ : Yojson.Safe.json = `Null

  let handle params =
    let%lwt result = G.handle params in
    match result with
    | {G.invalid_level = true} -> Jr.(Exception.raise_error Types.Error_code.Invalid_params)
    | _ -> Lwt.return_unit
end

let expose server =
  let module S = Jsonrpc_ocaml_yojson.Server in
  let module Factory = struct
    let create ~level ~body =
      let id = Uuidm.v4_gen (Random.get_state ()) () in
      T.Notification.make ~id ~level ~body
  end in
  let module NS = struct
    let send _ = Lwt.return_unit
  end in
  let module Notify_message_gateway =
    G.Notification.Notify_message.Make (U.Notification.Notify.Make (Factory) (NS)) in
  let module Notify_message = Procedure_intf.Make (Notify_message (Notify_message_gateway)) in
  let module E = Sxfiler_rpc.Endpoints in
  List.fold_left
    (fun server (name, handler) -> S.expose ~_method:name ~handler server)
    server
    [(E.Notification.Notify.endpoint, Notify_message.handler)]
