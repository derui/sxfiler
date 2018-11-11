(** this module defines JSON-RPC API utilities.*)
open Abbrevs

open Sxfiler_core
module J = Jsonrpc_ocaml_jsoo.Client
module T = Sxfiler_renderer_translator
include Filer_intf

module Make_api :
  J.Api_def with type params = E.Filer.Make.params and type result = E.Filer.Make.result = struct
  include E.Filer.Make

  type json = < > Js.t

  let name = endpoint

  let params_to_json v =
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

  let params_to_json v =
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

  let params_to_json v =
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

  let params_to_json v =
    Js.Unsafe.coerce
      (object%js
        val name = Js.string v.name

        val nodeId = Js.string v.node_id
      end)

  let result_of_json v = T.Filer.of_js @@ Js.Unsafe.coerce v
end

module Move_nodes_api :
  J.Api_def
  with type params = E.Filer.Move_nodes.params
   and type result = E.Filer.Move_nodes.result = struct
  include E.Filer.Move_nodes

  type json = < > Js.t

  let name = endpoint

  let params_to_json v =
    Js.Unsafe.coerce
      (object%js
        val workbenchId = Js.string v.workbench_id
      end)

  let result_of_json _ = ()
end

module Delete_nodes_api :
  J.Api_def
  with type params = E.Filer.Delete_nodes.params
   and type result = E.Filer.Delete_nodes.result = struct
  include E.Filer.Delete_nodes

  type json = < > Js.t

  let name = endpoint

  let params_to_json v =
    Js.Unsafe.coerce
      (object%js
        val workbenchId = Js.string v.workbench_id
      end)

  let result_of_json _ = ()
end

module Make (Client : C.Rpc_client.S) : S = struct
  let make params =
    let module R = Sxfiler_rpc in
    let module Jr = Jsonrpc_ocaml_jsoo in
    let%lwt response = Client.call ~api:(module Make_api) ~params () in
    match response with
    | Ok (Some v) -> Lwt.return_ok v
    | Ok None -> assert false
    | Error e when e.code = R.Errors.Filer.already_exists -> Lwt.return_error `Already_exists
    | Error e ->
      let message = Jr.Types.Error_code.to_message e.code in
      Lwt.fail Error.(to_exn @@ create message)

  let get params =
    let module R = Sxfiler_rpc in
    let%lwt response = Client.call ~api:(module Get_api) ~params () in
    match response with
    | Ok (Some v) -> Lwt.return_ok v
    | Ok None -> assert false
    | Error e when e.code = R.Errors.Filer.not_found -> Lwt.return_error `Not_found
    | Error e ->
      let module Jr = Jsonrpc_ocaml_jsoo in
      let message = Jr.Types.Error_code.to_message e.code in
      Lwt.fail Error.(to_exn @@ create message)

  (* commonly call API *)
  let call_api_only (type params result) (params : params)
      (module Api : J.Api_def with type params = params and type result = result) =
    let module R = Sxfiler_rpc in
    let%lwt response = Client.call ~api:(module Api) ~params () in
    match response with
    | Ok (Some v) -> Lwt.return v
    | Ok None -> assert false
    | Error e ->
      let module Jr = Jsonrpc_ocaml_jsoo in
      let message = Jr.Types.Error_code.to_message e.code in
      Lwt.fail Error.(to_exn @@ create message)

  let move_parent params = call_api_only params (module Move_parent_api)
  let enter_directory params = call_api_only params (module Enter_directory_api)
  let move_nodes params = call_api_only params (module Move_nodes_api)
  let delete_nodes params = call_api_only params (module Delete_nodes_api)
end
