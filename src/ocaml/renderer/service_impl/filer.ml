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

  let move_parent params =
    let waiter, wakener = Lwt.wait () in
    let module R = Sxfiler_rpc in
    let%lwt () =
      Client.request
        (module Move_parent_api)
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
end
