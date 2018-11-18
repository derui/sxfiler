(** this module defines JSON-RPC API utilities.*)
open Abbrevs

include Keymap_intf
module J = Jsonrpc_ocaml_jsoo.Client
module T = Sxfiler_renderer_translator

module Get_api :
  J.Api_def with type params = E.Keymap.Get.params and type result = E.Keymap.Get.result = struct
  include E.Keymap.Get

  type json = < > Js.t

  let name = endpoint
  let params_to_json _ = failwith "Do not pass parameters"
  let result_of_json v = Js.Unsafe.coerce v |> T.Key_map.of_js
end

module Add_context_api :
  J.Api_def
  with type params = E.Keymap.Add_context.params
   and type result = E.Keymap.Add_context.result = struct
  include E.Keymap.Add_context

  type json = < > Js.t

  let name = endpoint

  let params_to_json param =
    Js.Unsafe.coerce
      (object%js
        val context = Js.string param.context
      end)

  let result_of_json js = Js.Unsafe.coerce js |> T.Key_map.of_js
end

module Delete_context_api :
  J.Api_def
  with type params = E.Keymap.Delete_context.params
   and type result = E.Keymap.Delete_context.result = struct
  include E.Keymap.Delete_context

  type json = < > Js.t

  let name = endpoint

  let params_to_json param =
    Js.Unsafe.coerce
      (object%js
        val context = Js.string param.context
      end)

  let result_of_json js = Js.Unsafe.coerce js |> T.Key_map.of_js
end

module Make (Client : C.Rpc_client.S) : S = struct
  let get _ =
    let waiter, wakener = Lwt.wait () in
    let%lwt response = Client.call ~api:(module Get_api) () in
    ( match response with
      | Error _ | Ok None -> Lwt.wakeup_exn wakener Not_found
      | Ok (Some v) -> Lwt.wakeup wakener v ) ;
    waiter

  let add_context params =
    let waiter, wakener = Lwt.wait () in
    let%lwt response = Client.call ~api:(module Add_context_api) ~params () in
    ( match response with
      | Error _ | Ok None -> Lwt.wakeup_exn wakener Not_found
      | Ok (Some v) -> Lwt.wakeup wakener v ) ;
    waiter

  let delete_context params =
    let waiter, wakener = Lwt.wait () in
    let%lwt response = Client.call ~api:(module Delete_context_api) ~params () in
    ( match response with
      | Error _ | Ok None -> Lwt.wakeup_exn wakener Not_found
      | Ok (Some v) -> Lwt.wakeup wakener v ) ;
    waiter
end
