open Sxfiler_core
module D = Sxfiler_domain
module R = Sxfiler_rpc
module G = Sxfiler_generated
module Pb = Ocaml_protoc_plugin
module F = Sxfiler_workflow
module S = Sxfiler_dependency

let test_set =
  let filer = Test_fixtures.Filer.fixture () in
  let file_list_mock () =
    ( module struct
      let scan_location v =
        match v |> Path.to_string with
        | "left"  -> filer.left_file_window.file_list |> D.File_list.items |> Lwt_result.return
        | "right" -> filer.right_file_window.file_list |> D.File_list.items |> Lwt_result.return
        | _       -> failwith ""
    end : F.Common_step.File_list.Instance )
  in
  let filer_mock ?get () =
    ( module struct
      let get = Option.value ~default:(fun () -> failwith "get") get

      let copy_item _ = failwith ""

      let move_item _ = failwith ""

      let delete_item _ = failwith ""
    end : F.Common_step.Filer.Instance )
  in
  let filer = Test_fixtures.Filer.fixture () in
  [
    Alcotest_lwt.test_case "call initialize work flow with valid input" `Quick (fun _ () ->
        let work_flow _ = S.return [ F.Filer.Initialized filer ] in
        let request =
          {
            G.Service.Request.id = "id";
            command = G.Service.Command.FILER_INITIALIZE;
            payload =
              { G.Filer.InitializeRequest.left_location = "left"; right_location = "right" }
              |> G.Filer.InitializeRequest.to_proto |> Pb.Writer.contents |> Bytes.of_string;
          }
        in
        let%lwt res, events =
          R.Filer_endpoint.initialize work_flow
            (function
              | `Step_file_list_instance c -> S.Context.value (file_list_mock ()) c
              | `Step_filer_instance c     -> S.Context.value (filer_mock ()) c)
            request
        in
        Alcotest.(check & string) "same id" "id" res.id;
        Alcotest.(check & list & of_pp F.pp_event) "events" [ F.Filer (F.Filer.Initialized filer) ] events;
        Lwt.return_unit);
    Alcotest_lwt.test_case "call initialize work flow with invalid input" `Quick (fun _ () ->
        let work_flow _ = S.return [ F.Filer.Initialized filer ] in
        let request =
          { G.Service.Request.id = "id"; command = G.Service.Command.FILER_INITIALIZE; payload = Bytes.of_string "a" }
        in
        let%lwt res, events =
          R.Filer_endpoint.initialize work_flow
            (function
              | `Step_file_list_instance c -> S.Context.value (file_list_mock ()) c
              | `Step_filer_instance c     -> S.Context.value (filer_mock ()) c)
            request
        in
        Alcotest.(check & string) "same id" "id" res.id;
        Alcotest.(check & of_pp G.Service.Status.pp) "invalid" G.Service.Status.INVALID_REQUEST_PAYLOAD res.status;
        Alcotest.(check & list & of_pp F.pp_event) "events" [] events;
        Lwt.return_unit);
    Alcotest_lwt.test_case "call move work flow with marked" `Quick (fun _ () ->
        let input' = ref None in
        let expected_transfer_result =
          {
            F.Filer.source = Path.of_string "tmp" |> Result.get_ok;
            dest = Path.of_string "tmp" |> Result.get_ok;
            status = Success;
            timestamp = Time.of_float 1. |> Option.get;
          }
        in
        let work_flow input =
          input' := Some input;
          S.return_ok
            {
              F.Filer.Move.events =
                [
                  F.Filer.Updated (F.Filer.Left, filer.D.Filer.left_file_window |> D.File_window.as_free);
                  F.Filer.Updated (F.Filer.Right, filer.D.Filer.right_file_window |> D.File_window.as_free);
                ];
              result = expected_transfer_result;
            }
        in
        let request =
          {
            G.Service.Request.id = "id";
            command = G.Service.Command.FILER_MOVE;
            payload =
              { G.Filer.MoveRequest.transfer = Some { direction = LEFT_TO_RIGHT; target = ONE; target_id = "1" } }
              |> G.Filer.MoveRequest.to_proto |> Pb.Writer.contents |> Bytes.of_string;
          }
        in
        let%lwt res, events = R.Filer_endpoint.move work_flow (fun _ -> failwith "") request in
        let expected = Some { F.Filer.Move.target = D.File_item.Id.make "1"; direction = F.Filer.Left_to_right } in
        Alcotest.(check & string) "same id" "id" res.id;
        Alcotest.(check & option & of_pp F.Filer.Move.pp_input) "input" expected !input';
        Alcotest.(check & list & of_pp F.pp_event)
          "events"
          [
            F.Filer (F.Filer.Updated (F.Filer.Left, filer.D.Filer.left_file_window |> D.File_window.as_free));
            F.Filer (F.Filer.Updated (F.Filer.Right, filer.D.Filer.right_file_window |> D.File_window.as_free));
          ]
          events;
        Lwt.return_unit);
  ]
