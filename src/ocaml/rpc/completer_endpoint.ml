open Sxfiler_core
module F = Sxfiler_workflow
module G = Sxfiler_generated.Completer
module T = Sxfiler_translator
module S = Sxfiler_dependency

let to_global_event = List.map (fun e -> F.Completer e)

let initialize modules request =
  Endpoint.with_request (G.Completer.InitializeRequest.from_proto, G.Completer.InitializeResponse.to_proto) request
    ~f:(fun request ->
      let%lwt source =
        T.Completer.Collection.to_domain request.source
        |> Lwt_result.lift
        |> Lwt_result.map_err (fun _ -> Endpoint_error.Validation_error.make ~field:"source" ~message:"Invalid source")
      in
      match source with
      | Ok source ->
          let%lwt () = F.Completer.initialize { collection = source } |> S.provide modules |> S.run in
          Lwt.return_ok ((), [])
      | Error e   -> Lwt.return_error & Endpoint_error.invalid_input [ e ])

let complete modules request =
  Endpoint.with_request (G.Completer.CompleteRequest.from_proto, G.Completer.CompleteResponse.to_proto) request
    ~f:(fun request ->
      let%lwt events =
        F.Completer.complete { F.Completer.Complete.input = request.input } |> S.provide modules |> S.run
      in
      Lwt.return_ok ((), to_global_event events))
