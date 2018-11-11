(** this module defines JSON-RPC API utilities.*)

open Abbrevs
module J = Jsonrpc_ocaml_jsoo.Client
module T = Sxfiler_renderer_translator
include Completion_intf

module Setup_api :
  J.Api_def
  with type params = E.Completion.Setup.params
   and type result = E.Completion.Setup.result = struct
  include E.Completion.Setup

  type json = < > Js.t

  let name = endpoint

  let params_to_json v =
    let params =
      object%js
        val source = List.map T.Completion.Item.to_js v.source |> Array.of_list |> Js.array
      end
    in
    Js.Unsafe.coerce params

  let result_of_json _ = ()
end

module Read_api :
  J.Api_def with type params = E.Completion.Read.params and type result = E.Completion.Read.result =
struct
  include E.Completion.Read

  type json = < > Js.t

  let name = endpoint

  let params_to_json v =
    let params =
      object%js
        val input = Js.string v.input
      end
    in
    Js.Unsafe.coerce params

  let result_of_json v =
    let v = Js.Unsafe.coerce v in
    Array.to_list @@ Js.to_array @@ Js.array_map (fun v -> T.Completion.Candidate.of_js v) v
end

module Make (Client : C.Rpc_client.S) : S = struct
  let setup params =
    let%lwt response = Client.call ~api:(module Setup_api) ~params () in
    match response with
    (* TODO: should define original exception *)
    | Error _ -> Lwt.fail Not_found
    | Ok _ -> Lwt.return_unit

  let read params =
    let%lwt response = Client.call ~api:(module Read_api) ~params () in
    match response with Error _ | Ok None -> Lwt.fail Not_found | Ok (Some v) -> Lwt.return v
end
