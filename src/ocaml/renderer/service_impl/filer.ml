(** this module defines JSON-RPC API utilities.*)
open Abbrevs

include I.Filer
open Sxfiler_core
module J = Jsonrpc_ocaml_jsoo.Client
module T = Sxfiler_renderer_translator

module Make_api :
  J.Api_def with type params = E.Filer.Make.params and type result = E.Filer.Make.result = struct
  include E.Filer.Make

  type json = < > Js.t

  let name = endpoint

  let params_to_json params =
    let open Option.Infix in
    params
    >|= fun v ->
    Js.Unsafe.coerce
      (object%js
        val initialLocation = Js.string v.initial_location

        val name = Js.string v.name
      end)

  let result_of_json v = T.Filer.of_js @@ Js.Unsafe.coerce v
end

module Get_api :
  J.Api_def with type params = E.Filer.Get.params and type result = E.Filer.Get.result = struct
  include E.Filer.Get

  type json = < > Js.t

  let name = endpoint

  let params_to_json params =
    let open Option.Infix in
    params
    >|= fun v ->
    Js.Unsafe.coerce
      (object%js
        val name = Js.string v.name
      end)

  let result_of_json v = T.Filer.of_js @@ Js.Unsafe.coerce v
end

module Move_parent_api :
  J.Api_def
  with type params = E.Filer.Move_parent.params
   and type result = E.Filer.Move_parent.result = struct
  include E.Filer.Move_parent

  type json = < > Js.t

  let name = endpoint

  let params_to_json params =
    let open Option.Infix in
    params
    >|= fun v ->
    Js.Unsafe.coerce
      (object%js
        val name = Js.string v.name
      end)

  let result_of_json v = T.Filer.of_js @@ Js.Unsafe.coerce v
end

module Enter_directory_api :
  J.Api_def
  with type params = E.Filer.Enter_directory.params
   and type result = E.Filer.Enter_directory.result = struct
  include E.Filer.Enter_directory

  type json = < > Js.t

  let name = endpoint

  let params_to_json params =
    let open Option.Infix in
    params
    >|= fun v ->
    Js.Unsafe.coerce
      (object%js
        val name = Js.string v.name

        val nodeId = Js.string v.node_id
      end)

  let result_of_json v = T.Filer.of_js @@ Js.Unsafe.coerce v
end

module Plan_move_nodes_api :
  J.Api_def
  with type params = E.Filer.Plan_move_nodes.params
   and type result = E.Filer.Plan_move_nodes.result = struct
  include E.Filer.Plan_move_nodes

  type json = < > Js.t

  let name = endpoint

  let params_to_json params =
    let open Option.Infix in
    params
    >|= fun v ->
    Js.Unsafe.coerce
      (object%js
        val from = Js.string v.from

        val nodeIds = Fun.(List.map Js.string %> Array.of_list %> Js.array) v.node_ids

        val to_ = Js.string v._to
      end)

  let result_of_json v = T.Plan.of_js @@ Js.Unsafe.coerce v
end

module Make (Client : C.Rpc.Client) : S = struct
  let make params =
    let waiter, wakener = Lwt.wait () in
    let module R = Sxfiler_rpc in
    let module Jr = Jsonrpc_ocaml_jsoo in
    let%lwt () =
      Client.request
        (module Make_api)
        (Some params)
        (function
          | Ok (Some v) -> Lwt.wakeup wakener @@ Ok v
          | Ok None -> assert false
          | Error e when e.code = R.Errors.Filer.already_exists ->
            Lwt.wakeup wakener (Error `Already_exists)
          | Error e ->
            let message = Jr.Types.Error_code.to_message e.code in
            Lwt.wakeup_exn wakener Error.(to_exn @@ create message))
    in
    waiter

  let get params =
    let waiter, wakener = Lwt.wait () in
    let module R = Sxfiler_rpc in
    let%lwt () =
      Client.request
        (module Get_api)
        (Some params)
        (function
          | Ok (Some v) -> Lwt.wakeup wakener @@ Ok v
          | Ok None -> assert false
          | Error e when e.code = R.Errors.Filer.not_found -> Lwt.wakeup wakener (Error `Not_found)
          | Error e ->
            let module Jr = Jsonrpc_ocaml_jsoo in
            let message = Jr.Types.Error_code.to_message e.code in
            Lwt.wakeup_exn wakener Error.(to_exn @@ create message))
    in
    waiter

  (* commonly call API *)
  let call_api_only (type params result) (params : params)
      (module Api : J.Api_def with type params = params and type result = result) =
    let waiter, wakener = Lwt.wait () in
    let module R = Sxfiler_rpc in
    let%lwt () =
      Client.request
        (module Api)
        (Some params)
        (function
          | Ok (Some v) -> Lwt.wakeup wakener v
          | Ok None -> assert false
          | Error e ->
            let module Jr = Jsonrpc_ocaml_jsoo in
            let message = Jr.Types.Error_code.to_message e.code in
            Lwt.wakeup_exn wakener Error.(to_exn @@ create message))
    in
    waiter

  let move_parent params = call_api_only params (module Move_parent_api)
  let enter_directory params = call_api_only params (module Enter_directory_api)
  let plan_move_nodes params = call_api_only params (module Plan_move_nodes_api)
end
