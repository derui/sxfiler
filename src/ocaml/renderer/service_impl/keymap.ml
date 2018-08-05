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

module Make(Client:C.Rpc.Client) : S = struct
  let get _ =
    let waiter, wakener = Lwt.wait () in
    let%lwt () = Client.request (module Get_api) None (function
        | Error _ -> Lwt.wakeup_exn wakener Not_found
        | Ok v -> Lwt.wakeup wakener v)
    in waiter
end
