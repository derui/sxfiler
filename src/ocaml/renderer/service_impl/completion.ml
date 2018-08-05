(** this module defines JSON-RPC API utilities.*)

open Sxfiler_core
open Abbrevs
module J = Jsonrpc_ocaml_jsoo.Client
module T = Sxfiler_renderer_translator
include I.Completion

module Setup_api : J.Api_def with type params = E.Completion.Setup.params
                              and type result = E.Completion.Setup.result = struct
  include E.Completion.Setup
  type json = < > Js.t

  let name = endpoint
  let params_to_json params =
    let open Option.Infix in
    params >|= fun v ->
    let params = object%js
      val source = List.map T.Completion.Item.to_js v.source
                   |> Array.of_list
                   |> Js.array
    end in
    Js.Unsafe.coerce params

  let result_of_json _ = ()
end

module Read_api : J.Api_def with type params = E.Completion.Read.params
                         and type result = E.Completion.Read.result = struct
  include E.Completion.Read
  type json = < > Js.t

  let name = endpoint
  let params_to_json params =
    let open Option.Infix in
    params >|= fun v ->
    let params = object%js
      val input = Js.string v.input
    end in
    Js.Unsafe.coerce params

  let result_of_json v =
    let v = Js.Unsafe.coerce v in
    Array.to_list @@ Js.to_array @@ Js.array_map (fun v -> T.Completion.Candidate.of_js v) v
end


module Make(Client:C.Rpc.Client) : S = struct
  let setup params =
    let waiter, wakener = Lwt.wait () in
    let%lwt () = Client.request (module Setup_api) (Some params) (function
        (* TODO: should define original exception *)
        | Error _ -> Lwt.wakeup_exn wakener Not_found
        | Ok v -> Lwt.wakeup wakener v)
    in waiter

  let read params =
    let waiter, wakener = Lwt.wait () in
    let%lwt () = Client.request (module Read_api) (Some params) (function
        | Error _ -> Lwt.wakeup_exn wakener Not_found
        | Ok v -> Lwt.wakeup wakener v)
    in waiter
end
