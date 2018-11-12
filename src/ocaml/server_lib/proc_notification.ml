(** Proc_notification module defines functions for RPC of completion. *)
module SC = Sxfiler_server_core

module Jr = Jsonrpc_ocaml_yojson
module T = Sxfiler_domain
module U = Sxfiler_usecase
module G = Sxfiler_server_gateway
module P = Procedure
module E = Sxfiler_rpc.Endpoints

let notify_message_spec (module G : G.Notification.Notify_message.S) =
  let handle params =
    let%lwt result = G.handle params in
    match result with
    | {G.invalid_level = true} -> Jr.(Exception.raise_error Types.Error_code.Invalid_params)
    | _ -> Lwt.return_unit
  in
  P.to_procedure ~method_:E.Notification.Notify.endpoint
    ~spec:
      P.Spec.
        {params_of_json = `Required G.params_of_yojson; result_to_json = (fun _ -> `Null); handle}

let make_procedures (module NS : T.Notification_service.S) =
  let module I = Sxfiler_server_infra in
  let module S = Jsonrpc_ocaml_yojson.Server in
  let module Notify_message_gateway =
    G.Notification.Notify_message.Make (U.Notification.Notify.Make (I.Notification_factory) (NS)) in
  [notify_message_spec (module Notify_message_gateway)]
