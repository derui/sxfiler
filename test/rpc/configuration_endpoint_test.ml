open Sxfiler_core
module D = Sxfiler_domain
module R = Sxfiler_rpc
module G = Sxfiler_generated
module Pb = Ocaml_protoc_plugin
module F = Sxfiler_workflow
module T = Sxfiler_translator

let test_set =
  [
    Alcotest_lwt.test_case "get configuration from step" `Quick (fun _ () ->
        let request =
          { G.Service.Request.id = "id"; command = G.Service.Command.CONFIGURATION_GET; payload = Bytes.empty }
        in
        let actual = D.Configuration.default |> D.Configuration.current_theme "sample" in
        let expected = T.Configuration.of_domain actual in
        let step () = Lwt.return actual in
        let%lwt res, events = R.Configuration_endpoint.get step request in
        let payload =
          res.payload |> Bytes.to_string |> Pb.Reader.create |> G.Configuration.GetResponse.from_proto |> Result.get_ok
        in
        Alcotest.(check & string) "same id" "id" res.id;
        Alcotest.(check & list & of_pp F.pp_event) "events" [] events;
        Alcotest.(check & of_pp G.Service.Status.pp) "invalid" G.Service.Status.SUCCESS res.status;
        Alcotest.(check & option & testable G.Configuration.Configuration.pp G.Configuration.Configuration.equal)
          "configuration" (Some expected) payload.configuration;
        Lwt.return_unit);
  ]
