(** this module defines JSON-RPC API utilities.*)
open Abbrevs

open Sxfiler_core
include Plan_intf
module J = Jsonrpc_ocaml_jsoo.Client
module T = Sxfiler_renderer_translator

module Reject_api :
  J.Api_def with type params = E.Plan.Reject.params and type result = E.Plan.Reject.result = struct
  include E.Plan.Reject

  type json = < > Js.t

  let name = endpoint

  let params_to_json v =
    Js.Unsafe.coerce
      (object%js
        val workbenchId = Js.string v.workbench_id
      end)

  let result_of_json _ = ()
end

module Plan_move_nodes_api :
  J.Api_def
  with type params = E.Plan.Filer.Move_nodes.params
   and type result = E.Plan.Filer.Move_nodes.result = struct
  include E.Plan.Filer.Move_nodes

  type json = < > Js.t

  let name = endpoint

  let params_to_json v =
    Js.Unsafe.coerce
      (object%js
        val from = Js.string v.from

        val nodeIds = Fun.(List.map Js.string %> Array.of_list %> Js.array) v.node_ids

        val to_ = Js.string v._to
      end)

  let result_of_json v = T.Plan.of_js @@ Js.Unsafe.coerce v
end

module Plan_delete_nodes_api :
  J.Api_def
  with type params = E.Plan.Filer.Delete_nodes.params
   and type result = E.Plan.Filer.Delete_nodes.result = struct
  include E.Plan.Filer.Delete_nodes

  type json = < > Js.t

  let name = endpoint

  let params_to_json v =
    Js.Unsafe.coerce
      (object%js
        val from = Js.string v.from

        val nodeIds = Fun.(List.map Js.string %> Array.of_list %> Js.array) v.node_ids
      end)

  let result_of_json v = T.Plan.of_js @@ Js.Unsafe.coerce v
end

module Make (Client : C.Rpc_client.S) : S = struct
  (* commonly call API *)
  let call_api_only (type params result) (params : params)
      (module Api : J.Api_def with type params = params and type result = result) =
    let waiter, wakener = Lwt.wait () in
    let module R = Sxfiler_rpc in
    let%lwt response = Client.call ~api:(module Api) ~params () in
    ( match response with
      | Ok (Some v) -> Lwt.wakeup wakener v
      | Ok None -> assert false
      | Error e ->
        let module Jr = Jsonrpc_ocaml_jsoo in
        let message = Jr.Types.Error_code.to_message e.code in
        Lwt.wakeup_exn wakener Error.(to_exn @@ create message) ) ;
    waiter

  let reject params = call_api_only params (module Reject_api)
  let plan_move_nodes params = call_api_only params (module Plan_move_nodes_api)
  let plan_delete_nodes params = call_api_only params (module Plan_delete_nodes_api)
end
