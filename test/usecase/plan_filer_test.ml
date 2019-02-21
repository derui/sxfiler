open Sxfiler_core
module D = Sxfiler_domain
module U = Sxfiler_usecase
module TF = Test_fixtures
module TS = Test_fixtures.Service

let stat_base = TF.File_stat.fixture ()
let dir_stat = TF.File_stat.fixture ~directory:true ()

let move_plan_test_set =
  [ Alcotest_lwt.test_case "move_nodes plan issue error when workbench not found" `Quick
      (fun _ () ->
         let module FR = (val TF.Memory_repository.filer_repository ()) in
         let pf = TF.Plan.dummy_factory "id" in
         let module Usecase =
           U.Plan.Filer.Make_move_plan.Make (FR) ((val pf)) (TS.Node_transport_service)
             ((val TS.location_scanner_service Path.(of_string "foo") []))
         in
         let%lwt result = Usecase.execute {source = "src"; dest = "dest"; node_ids = []} in
         Alcotest.(check @@ result (of_pp Fmt.nop) (of_pp Fmt.nop))
           "not found workbench"
           (Error (`Not_found "src"))
           result ;
         Lwt.return_unit )
  ; Alcotest_lwt.test_case "get the plan to move nodes" `Quick (fun _ () ->
        let nodes = [TF.Node.fixture ~full_path:Path.(of_string "foo") dir_stat] in
        let source_file_tree = D.File_tree.make ~location:(Path.of_string "/foo") ~nodes in
        let dest_file_tree = D.File_tree.make ~location:(Path.of_string "/bar") ~nodes in
        let source_filer =
          TF.Filer.fixture "src" ~file_tree:source_file_tree ~sort_order:D.Types.Sort_type.Name
        in
        let dest_filer =
          TF.Filer.fixture "dest" ~file_tree:dest_file_tree ~sort_order:D.Types.Sort_type.Name
        in
        let module FR =
          (val TF.Memory_repository.filer_repository ~initial:[source_filer; dest_filer] ())
        in
        let pf = TF.Plan.dummy_factory "id" in
        let module Usecase =
          U.Plan.Filer.Make_move_plan.Make (FR) ((val pf)) (TS.Node_transport_service)
            ((val TS.location_scanner_service Path.(of_string "foo") []))
        in
        match%lwt
          Usecase.execute {source = "src"; dest = "dest"; node_ids = List.map D.Node.id nodes}
        with
        | Error _ -> Alcotest.fail "Unknown error"
        | Ok plan ->
          Alcotest.(check string) "plan" "id" plan.D.Plan.id ;
          Lwt.return_unit )
  ; Alcotest_lwt.test_case "plan to move nodes returns conflict to move between same location"
      `Quick (fun _ () ->
          let nodes = [TF.Node.fixture ~full_path:Path.(of_string "foo") dir_stat] in
          let source_file_tree = D.File_tree.make ~location:(Path.of_string "/foo") ~nodes in
          let dest_file_tree = D.File_tree.make ~location:(Path.of_string "/bar") ~nodes in
          let source_filer =
            TF.Filer.fixture "src" ~file_tree:source_file_tree ~sort_order:D.Types.Sort_type.Name
          in
          let dest_filer =
            TF.Filer.fixture "dest" ~file_tree:dest_file_tree ~sort_order:D.Types.Sort_type.Name
          in
          let pf = TF.Plan.dummy_factory "id" in
          let module FR =
            (val TF.Memory_repository.filer_repository ~initial:[source_filer; dest_filer] ())
          in
          let module Usecase =
            U.Plan.Filer.Make_move_plan.Make (FR) ((val pf)) (TS.Node_transport_service)
              ((val TS.location_scanner_service Path.(of_string "foo") []))
          in
          let param = {Usecase.source = "src"; dest = "dest"; node_ids = List.map D.Node.id nodes} in
          match%lwt Usecase.execute param with
          | Error _ -> Alcotest.fail "Unknown error"
          | Ok plan ->
            let expected = [D.Plan.Target_node.need_fix @@ D.Node.id @@ List.hd nodes] in
            Alcotest.(check @@ list @@ of_pp D.Plan.Target_node.pp)
              "plan" expected plan.target_nodes ;
            Lwt.return_unit ) ]

let delete_plan_test_set =
  [ Alcotest_lwt.test_case "delete_nodes plan issue error when workbench not found" `Quick
      (fun _ () ->
         let pf = TF.Plan.dummy_factory "id" in
         let module FR = (val TF.Memory_repository.filer_repository ()) in
         let module Usecase =
           U.Plan.Filer.Make_delete_plan.Make (FR) ((val pf))
             ((val TS.location_scanner_service Path.(of_string "foo") []))
             (TS.Node_trash_service)
         in
         let%lwt result = Usecase.execute {source = "source"; node_ids = ["node"]} in
         Alcotest.(check @@ result (of_pp Fmt.nop) (of_pp Fmt.nop))
           "not found filer"
           (Error (`Not_found "source"))
           result ;
         Lwt.return_unit )
  ; Alcotest_lwt.test_case "get the plan to delete nodes" `Quick (fun _ () ->
        let nodes = [TF.Node.fixture ~full_path:Path.(of_string "foo") dir_stat] in
        let source_file_tree = D.File_tree.make ~location:(Path.of_string "/foo") ~nodes in
        let source_filer =
          TF.Filer.fixture "source" ~file_tree:source_file_tree ~sort_order:D.Types.Sort_type.Name
        in
        let pf = TF.Plan.dummy_factory "id" in
        let module FR = (val TF.Memory_repository.filer_repository ~initial:[source_filer] ()) in
        let module Usecase =
          U.Plan.Filer.Make_delete_plan.Make (FR) ((val pf))
            ((val TS.location_scanner_service Path.(of_string "foo") []))
            (TS.Node_trash_service)
        in
        let%lwt result =
          Usecase.execute {source = "source"; node_ids = List.map D.Node.id nodes}
        in
        let plan = Result.get result |> D.Plan.target_nodes in
        let expected = [D.Plan.Target_node.no_problem (List.nth nodes 0 |> D.Node.id)] in
        Alcotest.(check @@ list @@ of_pp D.Plan.Target_node.pp) "plan" expected plan ;
        Lwt.return_unit ) ]

let test_set = move_plan_test_set @ delete_plan_test_set
