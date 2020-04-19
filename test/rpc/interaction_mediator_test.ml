open Sxfiler_core
module D = Sxfiler_domain
module R = Sxfiler_rpc
module G = Sxfiler_generated
module Pb = Ocaml_protoc_plugin

let filer_copy_test =
  [
    Alcotest_lwt.test_case "user decide to overwrite an item in copy" `Quick (fun _ () ->
        let item = Test_fixtures.File_item.fixture () in
        let expected = { G.Filer.CopyUserDecisionResponse.action = OVERWRITE; new_name = "" } in
        let module I = struct
          module Client = struct
            type t = unit

            let call () command _ =
              Alcotest.(check @@ of_pp G.Service.Command.pp)
                "command" G.Service.Command.FILER_COPY_INTERACTION command.R.Client_command.command;
              G.Filer.CopyUserDecisionResponse.to_proto expected
              |> Pb.Writer.contents |> Pb.Reader.create |> command.reader
              |> Result.map_error (fun e -> `Pb_error e)
              |> Lwt.return
          end

          let instance = ()
        end in
        let module M = (val R.Interaction_mediator.make (module I)) in
        let%lwt res = M.(Mediator.require_action instance ~command:(D.Interaction.Filer_copy item)) in
        Alcotest.(check @@ of_pp D.Interaction.pp_event)
          "event"
          D.Interaction.(Filer_copy_selected Filer_copy_selected.Overwrite)
          res;
        Lwt.return_unit);
    Alcotest_lwt.test_case "use decide to cancel " `Quick (fun _ () ->
        let item = Test_fixtures.File_item.fixture () in
        let expected = { G.Filer.CopyUserDecisionResponse.action = CANCEL; new_name = "" } in
        let module I = struct
          module Client = struct
            type t = unit

            let call () command _ =
              Alcotest.(check @@ of_pp G.Service.Command.pp)
                "command" G.Service.Command.FILER_COPY_INTERACTION command.R.Client_command.command;
              G.Filer.CopyUserDecisionResponse.to_proto expected
              |> Pb.Writer.contents |> Pb.Reader.create |> command.reader
              |> Result.map_error (fun e -> `Pb_error e)
              |> Lwt.return
          end

          let instance = ()
        end in
        let module M = (val R.Interaction_mediator.make (module I)) in
        let%lwt res = M.(Mediator.require_action instance ~command:(D.Interaction.Filer_copy item)) in
        Alcotest.(check @@ of_pp D.Interaction.pp_event) "event" D.Interaction.Canceled res;
        Lwt.return_unit);
    Alcotest_lwt.test_case "cancel if new name is empty when action is rename" `Quick (fun _ () ->
        let item = Test_fixtures.File_item.fixture () in
        let expected = { G.Filer.CopyUserDecisionResponse.action = RENAME; new_name = "" } in
        let module I = struct
          module Client = struct
            type t = unit

            let call () command _ =
              Alcotest.(check @@ of_pp G.Service.Command.pp)
                "command" G.Service.Command.FILER_COPY_INTERACTION command.R.Client_command.command;
              G.Filer.CopyUserDecisionResponse.to_proto expected
              |> Pb.Writer.contents |> Pb.Reader.create |> command.reader
              |> Result.map_error (fun e -> `Pb_error e)
              |> Lwt.return
          end

          let instance = ()
        end in
        let module M = (val R.Interaction_mediator.make (module I)) in
        let%lwt res = M.(Mediator.require_action instance ~command:(D.Interaction.Filer_copy item)) in
        Alcotest.(check @@ of_pp D.Interaction.pp_event) "event" D.Interaction.Canceled res;
        Lwt.return_unit);
  ]

and filer_move_test =
  [
    Alcotest_lwt.test_case "user decide to overwrite an item in move" `Quick (fun _ () ->
        let item = Test_fixtures.File_item.fixture () in
        let expected = { G.Filer.MoveUserDecisionResponse.action = OVERWRITE; new_name = "" } in
        let module I = struct
          module Client = struct
            type t = unit

            let call () command _ =
              Alcotest.(check @@ of_pp G.Service.Command.pp)
                "command" G.Service.Command.FILER_MOVE_INTERACTION command.R.Client_command.command;
              G.Filer.MoveUserDecisionResponse.to_proto expected
              |> Pb.Writer.contents |> Pb.Reader.create |> command.reader
              |> Result.map_error (fun e -> `Pb_error e)
              |> Lwt.return
          end

          let instance = ()
        end in
        let module M = (val R.Interaction_mediator.make (module I)) in
        let%lwt res = M.(Mediator.require_action instance ~command:(D.Interaction.Filer_move item)) in
        Alcotest.(check @@ of_pp D.Interaction.pp_event)
          "event"
          D.Interaction.(Filer_move_selected Filer_move_selected.Overwrite)
          res;
        Lwt.return_unit);
    Alcotest_lwt.test_case "use decide to cancel " `Quick (fun _ () ->
        let item = Test_fixtures.File_item.fixture () in
        let expected = { G.Filer.MoveUserDecisionResponse.action = CANCEL; new_name = "" } in
        let module I = struct
          module Client = struct
            type t = unit

            let call () command _ =
              Alcotest.(check @@ of_pp G.Service.Command.pp)
                "command" G.Service.Command.FILER_MOVE_INTERACTION command.R.Client_command.command;
              G.Filer.MoveUserDecisionResponse.to_proto expected
              |> Pb.Writer.contents |> Pb.Reader.create |> command.reader
              |> Result.map_error (fun e -> `Pb_error e)
              |> Lwt.return
          end

          let instance = ()
        end in
        let module M = (val R.Interaction_mediator.make (module I)) in
        let%lwt res = M.(Mediator.require_action instance ~command:(D.Interaction.Filer_move item)) in
        Alcotest.(check @@ of_pp D.Interaction.pp_event) "event" D.Interaction.Canceled res;
        Lwt.return_unit);
    Alcotest_lwt.test_case "cancel if new name is empty when action is rename" `Quick (fun _ () ->
        let item = Test_fixtures.File_item.fixture () in
        let expected = { G.Filer.MoveUserDecisionResponse.action = RENAME; new_name = "" } in
        let module I = struct
          module Client = struct
            type t = unit

            let call () command _ =
              Alcotest.(check @@ of_pp G.Service.Command.pp)
                "command" G.Service.Command.FILER_MOVE_INTERACTION command.R.Client_command.command;
              G.Filer.MoveUserDecisionResponse.to_proto expected
              |> Pb.Writer.contents |> Pb.Reader.create |> command.reader
              |> Result.map_error (fun e -> `Pb_error e)
              |> Lwt.return
          end

          let instance = ()
        end in
        let module M = (val R.Interaction_mediator.make (module I)) in
        let%lwt res = M.(Mediator.require_action instance ~command:(D.Interaction.Filer_move item)) in
        Alcotest.(check @@ of_pp D.Interaction.pp_event) "event" D.Interaction.Canceled res;
        Lwt.return_unit);
  ]

and filer_delete_test =
  [
    Alcotest_lwt.test_case "user decide to overwrite an item in delete" `Quick (fun _ () ->
        let item = Test_fixtures.File_item.fixture () in
        let expected = { G.Filer.DeleteUserDecisionResponse.confirmed = true } in
        let module I = struct
          module Client = struct
            type t = unit

            let call () command _ =
              Alcotest.(check @@ of_pp G.Service.Command.pp)
                "command" G.Service.Command.FILER_DELETE_INTERACTION command.R.Client_command.command;
              G.Filer.DeleteUserDecisionResponse.to_proto expected
              |> Pb.Writer.contents |> Pb.Reader.create |> command.reader
              |> Result.map_error (fun e -> `Pb_error e)
              |> Lwt.return
          end

          let instance = ()
        end in
        let module M = (val R.Interaction_mediator.make (module I)) in
        let%lwt res = M.(Mediator.require_action instance ~command:(D.Interaction.Filer_delete item)) in
        Alcotest.(check @@ of_pp D.Interaction.pp_event)
          "event"
          D.Interaction.(Filer_delete_selected Filer_delete_selected.Confirm)
          res;
        Lwt.return_unit);
    Alcotest_lwt.test_case "use decide to cancel " `Quick (fun _ () ->
        let item = Test_fixtures.File_item.fixture () in
        let expected = { G.Filer.DeleteUserDecisionResponse.confirmed = false } in
        let module I = struct
          module Client = struct
            type t = unit

            let call () command _ =
              Alcotest.(check @@ of_pp G.Service.Command.pp)
                "command" G.Service.Command.FILER_DELETE_INTERACTION command.R.Client_command.command;
              G.Filer.DeleteUserDecisionResponse.to_proto expected
              |> Pb.Writer.contents |> Pb.Reader.create |> command.reader
              |> Result.map_error (fun e -> `Pb_error e)
              |> Lwt.return
          end

          let instance = ()
        end in
        let module M = (val R.Interaction_mediator.make (module I)) in
        let%lwt res = M.(Mediator.require_action instance ~command:(D.Interaction.Filer_delete item)) in
        Alcotest.(check @@ of_pp D.Interaction.pp_event) "event" D.Interaction.Canceled res;
        Lwt.return_unit);
  ]

let test_set = List.concat [ filer_copy_test; filer_move_test; filer_delete_test ]
