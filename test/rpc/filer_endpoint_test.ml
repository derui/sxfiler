open Sxfiler_core
module D = Sxfiler_domain
module R = Sxfiler_rpc
module G = Sxfiler_generated
module Pb = Ocaml_protoc_plugin
module F = Sxfiler_workflow

let test_set =
  let filer = Test_fixtures.Filer.fixture () in
  [
    Alcotest_lwt.test_case "call initialize work flow with valid input" `Quick (fun _ () ->
        let work_flow _ = Lwt.return [ F.Filer.Updated filer ] in
        let request =
          {
            G.Service.Request.id = "id";
            command = G.Service.Command.FILER_INITIALIZE;
            payload =
              { G.Filer.InitializeRequest.left_location = "left"; right_location = "right" }
              |> G.Filer.InitializeRequest.to_proto |> Pb.Writer.contents |> Bytes.of_string;
          }
        in
        let%lwt res, events = R.Filer_endpoint.initialize work_flow request in
        Alcotest.(check & string) "same id" "id" res.id;
        Alcotest.(check & list & of_pp F.pp_event) "events" [ F.Filer (F.Filer.Updated filer) ] events;
        Lwt.return_unit);
    Alcotest_lwt.test_case "call initialize work flow with invalid input" `Quick (fun _ () ->
        let work_flow _ = Lwt.return [ F.Filer.Updated filer ] in
        let request =
          { G.Service.Request.id = "id"; command = G.Service.Command.FILER_INITIALIZE; payload = Bytes.of_string "a" }
        in
        let%lwt res, events = R.Filer_endpoint.initialize work_flow request in
        Alcotest.(check & string) "same id" "id" res.id;
        Alcotest.(check & of_pp G.Service.Status.pp) "invalid" G.Service.Status.INVALID_REQUEST_PAYLOAD res.status;
        Alcotest.(check & list & of_pp F.pp_event) "events" [] events;
        Lwt.return_unit);
    Alcotest_lwt.test_case "call move work flow with marked" `Quick (fun _ () ->
        let input' = ref None in
        let work_flow input =
          input' := Some input;
          Lwt.return [ F.Filer.Updated filer ]
        in
        let get () = Lwt.return_some filer in
        let request =
          {
            G.Service.Request.id = "id";
            command = G.Service.Command.FILER_MOVE;
            payload =
              { G.Filer.MoveRequest.direction = LEFT_TO_RIGHT; target = MARKED; target_id = "" }
              |> G.Filer.MoveRequest.to_proto |> Pb.Writer.contents |> Bytes.of_string;
          }
        in
        let%lwt res, events = R.Filer_endpoint.move get work_flow request in
        let expected = Some { F.Filer.Move.target = Marked; direction = F.Filer.Left_to_right; filer } in
        Alcotest.(check & string) "same id" "id" res.id;
        Alcotest.(check & option & of_pp F.Filer.Move.pp_input) "input" expected !input';
        Alcotest.(check & list & of_pp F.pp_event) "events" [ F.Filer (F.Filer.Updated filer) ] events;
        Lwt.return_unit);
  ]
