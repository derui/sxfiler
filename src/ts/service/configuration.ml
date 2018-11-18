(** this module defines JSON-RPC API utilities.*)
open Abbrevs

include Configuration_intf
open Sxfiler_core
module J = Jsonrpc_ocaml_jsoo.Client
module T = Sxfiler_renderer_translator

module Get_api :
  J.Api_def
  with type params = E.Configuration.Get.params
   and type result = E.Configuration.Get.result = struct
  include E.Configuration.Get

  type json = < > Js.t

  let name = endpoint
  let params_to_json _ = raise Error.(create "Do not call with parameter" |> to_exn)
  let result_of_json v = T.Configuration.of_js @@ Js.Unsafe.coerce v
end

module Make (Client : C.Rpc_client.S) : S = struct
  let get _ =
    let%lwt response = Client.call ~api:(module Get_api) () in
    match response with Error _ | Ok None -> Lwt.fail Not_found | Ok (Some v) -> Lwt.return v
end
