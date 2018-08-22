(** this module defines JSON-RPC API utilities.*)
open Abbrevs
include I.Keymap

module J = Jsonrpc_ocaml_jsoo.Client
module T = Sxfiler_renderer_translator

module Get_api : J.Api_def with type params = E.Keymap.Get.params
                            and type result = E.Keymap.Get.result = struct
  include E.Keymap.Get
  type json = < > Js.t

  let name = endpoint
  let params_to_json _ = None

  let result_of_json v =
    let v = Js.Unsafe.coerce v in T.Key_map.of_js v
end

module Enable_context_api : J.Api_def with type params = E.Keymap.Enable_context.params
                                       and type result = E.Keymap.Enable_context.result = struct
  include E.Keymap.Enable_context
  type json = < > Js.t
  let name = endpoint

  let params_to_json param =
    let open Sxfiler_core.Option.Infix in
    param >|= fun param ->
    Js.Unsafe.coerce (object%js
      val context = Js.string param.context
    end)

  let result_of_json js = T.Key_map.of_js @@ Js.Unsafe.coerce js
end

module Disable_context_api : J.Api_def with type params = E.Keymap.Disable_context.params
                                       and type result = E.Keymap.Disable_context.result = struct
  include E.Keymap.Disable_context
  type json = < > Js.t
  let name = endpoint

  let params_to_json param =
    let open Sxfiler_core.Option.Infix in
    param >|= fun param ->
    Js.Unsafe.coerce (object%js
      val context = Js.string param.context
    end)

  let result_of_json js = T.Key_map.of_js @@ Js.Unsafe.coerce js
end

module Make(Client:C.Rpc.Client) : S = struct
  let get _ =
    let waiter, wakener = Lwt.wait () in
    let%lwt () = Client.request (module Get_api) None (function
        | Error _ | Ok None -> Lwt.wakeup_exn wakener Not_found
        | Ok (Some v) -> Lwt.wakeup wakener v)
    in waiter

  let enable_context param =
    let waiter, wakener = Lwt.wait () in
    let%lwt () = Client.request (module Enable_context_api) (Some param) (function
        | Error _ | Ok None -> Lwt.wakeup_exn wakener Not_found
        | Ok (Some v) -> Lwt.wakeup wakener v)
    in waiter

  let disable_context param =
    let waiter, wakener = Lwt.wait () in
    let%lwt () = Client.request (module Disable_context_api) (Some param) (function
        | Error _ | Ok None -> Lwt.wakeup_exn wakener Not_found
        | Ok (Some v) -> Lwt.wakeup wakener v)
    in waiter
end
