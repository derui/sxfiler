open Sxfiler_core
module F = Sxfiler_workflow
module G = Sxfiler_generated.Completer
module T = Sxfiler_translator

type initialize = F.Completer.Initialize.work_flow -> Endpoint.t

let to_global_event = List.map (fun e -> F.Completer e)

let initialize : initialize =
 fun flow request ->
  Endpoint.with_request (G.Completer.InitializeRequest.from_proto, G.Completer.InitializeResponse.to_proto) request
    ~f:(fun request ->
      let source =
        T.Completer.Collection.to_domain request.source
        |> Lwt_result.lift
        |> Lwt_result.map_err (fun _ -> Endpoint_error.Validation_error.make ~field:"source" ~message:"Invalid source")
      in
      let open Lwt_result.Infix in
      let%lwt v = source >>= fun v -> flow { collection = v } |> Lwt_result.ok in
      match v with Error e -> Lwt.return_error & Endpoint_error.invalid_input [ e ] | Ok () -> Lwt.return_ok ((), []))

type complete = F.Completer.Complete.work_flow -> Endpoint.t

let complete : complete =
 fun flow request ->
  Endpoint.with_request (G.Completer.CompleteRequest.from_proto, G.Completer.CompleteResponse.to_proto) request
    ~f:(fun request ->
      let%lwt events = flow { F.Completer.Complete.input = request.input } in
      Lwt.return_ok ((), to_global_event events))
