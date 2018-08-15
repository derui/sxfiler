(** this module defines JSON-RPC API utilities.*)
open Abbrevs
include I.Configuration

module J = Jsonrpc_ocaml_jsoo.Client
module T = Sxfiler_renderer_translator

module Get_api : J.Api_def with type params = E.Configuration.Get.params
                            and type result = E.Configuration.Get.result = struct
  include E.Configuration.Get
  type json = < > Js.t
  let name = endpoint

  let params_to_json _ = None
  let result_of_json v = T.Configuration.of_js @@ Js.Unsafe.coerce v
end

module Make(Client:C.Rpc.Client) : S = struct
  let get _ =
    let waiter, wakener = Lwt.wait () in
    let%lwt () = Client.request (module Get_api) None (function
        | Error _ | Ok None -> Lwt.wakeup_exn wakener Not_found
        | Ok (Some v) -> Lwt.wakeup wakener v)
    in waiter
end
