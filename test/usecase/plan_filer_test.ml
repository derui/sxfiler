open Sxfiler_core
module D = Sxfiler_domain
module U = Sxfiler_usecase

let stat_base = Test_fixtures.File_stat.fixture ()
let dir_stat = Test_fixtures.File_stat.fixture ~directory:true ()

let node_base ~id ?(full_path = Path.of_string "foo") ?(stat = stat_base) ?(link_path = None) () =
  D.Node.make ~id ~full_path ~stat ~link_path

module Dummy_transport_service : D.Node_transporter_service.S = struct
  let transport ?new_name ~node ~_to () = Lwt.return_unit
end

module Dummy_location_scanner_service : D.Location_scanner_service.S = struct
  let scan _ = failwith ""
end

let move_plan_test_set =
  [ Alcotest_lwt.test_case "move_nodes plan issue error when workbench not found" `Quick
      (fun _ () ->
         let module FR = struct
           let resolve _ = Lwt.return_none
           let store _ = assert false
           let remove _ = assert false
         end in
         let pf = Test_fixtures.Plan.dummy_factory "id" in
         let module Usecase =
           U.Plan.Filer.Make_move_plan.Make (FR) ((val pf)) (Dummy_transport_service)
             (Dummy_location_scanner_service)
         in
         let%lwt result = Usecase.execute {source = "src"; dest = "dest"; node_ids = []} in
         Alcotest.(check @@ result (of_pp Fmt.nop) (of_pp Fmt.nop))
           "not found workbench"
           (Error (`Not_found "src"))
           result ;
         Lwt.return_unit )
  ; Alcotest_lwt.test_case "get the plan to move nodes" `Quick (fun _ () ->
        let nodes = [Test_fixtures.Node.fixture ~full_path:Path.(of_string "foo") dir_stat] in
        let source_file_tree = D.File_tree.make ~location:(Path.of_string "/foo") ~nodes in
        let dest_file_tree = D.File_tree.make ~location:(Path.of_string "/bar") ~nodes in
        let source_filer =
          D.Filer.make ~id:"id" ~file_tree:source_file_tree ~selected_nodes:[]
            ~history:D.Location_history.(make ())
            ~sort_order:D.Types.Sort_type.Name ()
        in
        let dest_filer =
          D.Filer.make ~id:"id" ~file_tree:dest_file_tree ~selected_nodes:[]
            ~history:D.Location_history.(make ())
            ~sort_order:D.Types.Sort_type.Name ()
        in
        let module FR = struct
          let resolve id =
            match id with
            | "source" -> Lwt.return_some source_filer
            | "dest" -> Lwt.return_some dest_filer
            | _ -> Lwt.return_none

          let store _ = assert false
          let remove _ = assert false
        end in
        let pf = Test_fixtures.Plan.dummy_factory "id" in
        let module Usecase =
          U.Plan.Filer.Make_move_plan.Make (FR) ((val pf)) (Dummy_transport_service)
            (Dummy_location_scanner_service)
        in
        match%lwt
          Usecase.execute {source = "src"; dest = "dest"; node_ids = List.map D.Node.id nodes}
        with
        | Error _ -> Alcotest.fail "Unknown error"
        | Ok plan ->
          Alcotest.(check string) "plan" "id" plan.D.id ;
          Lwt.return_unit )
  ; Alcotest_lwt.test_case "plan to move nodes returns conflict to move between same location"
      `Quick (fun _ () ->
          let nodes = [node_base ~id:"bar" ~stat:dir_stat ()] in
          let source_filer =
            D.Filer.make ~id:"foo"
              ~location:Path.(of_string "/bar")
              ~nodes
              ~history:D.Location_history.(make ())
              ~sort_order:D.Types.Sort_type.Name
          in
          let dest_filer =
            D.Filer.make ~id:"bar"
              ~location:Path.(of_string "/bar")
              ~nodes:[]
              ~history:D.Location_history.(make ())
              ~sort_order:D.Types.Sort_type.Name
          in
          let id = Uuidm.v4_gen (Random.get_state ()) () in
          let env = D.Workbench.{source = source_filer; nodes; dest = dest_filer} in
          let module WR = struct
            let resolve _ = Lwt.return_some (D.Workbench.make ~id ~env ~corrections:[])
            let store _ = assert false
            let remove _ = assert false
          end in
          let module Usecase = U.Plan.Filer.Move_nodes.Make (WR) in
          match%lwt Usecase.execute {workbench_id = id} with
          | Error _ -> Alcotest.fail "Unknown error"
          | Ok plan ->
            let expected =
              D.Plan.
                { workbench_id = id
                ; source = [Fun.(List.hd %> node_to_conflict) nodes]
                ; dest = [Fun.(List.hd %> node_to_conflict) nodes] }
            in
            Alcotest.(check (of_pp D.Plan.pp)) "plan" expected plan ;
            Lwt.return_unit ) ]

let delete_plan_test_set =
  [ Alcotest_lwt.test_case "delete_nodes plan issue error when workbench not found" `Quick
      (fun _ () ->
         let module WR = struct
           let resolve _ = Lwt.return_none
           let store _ = assert false
           let remove _ = assert false
         end in
         let module Usecase = U.Plan.Filer.Delete_nodes.Make (WR) in
         let%lwt result = Usecase.execute {workbench_id = Uuidm.v4_gen (Random.get_state ()) ()} in
         Alcotest.(check @@ result (of_pp Fmt.nop) (of_pp Fmt.nop))
           "not found workbench"
           (Error `Not_found_wb)
           result ;
         Lwt.return_unit )
  ; Alcotest_lwt.test_case "get the plan to delete nodes" `Quick (fun _ () ->
        let nodes = [node_base ~id:"bar" ~stat:dir_stat ()] in
        let source_filer =
          D.Filer.make ~id:"foo"
            ~location:Path.(of_string "/bar")
            ~nodes
            ~history:D.Location_history.(make ())
            ~sort_order:D.Types.Sort_type.Name
        in
        let id = Uuidm.v4_gen (Random.get_state ()) () in
        let env = D.Workbench.{source = source_filer; nodes; dest = source_filer} in
        let module WR = struct
          let resolve _ = Lwt.return_some (D.Workbench.make ~id ~env ~corrections:[])
          let store _ = assert false
          let remove _ = assert false
        end in
        let module Usecase = U.Plan.Filer.Delete_nodes.Make (WR) in
        let%lwt ret = Usecase.execute {workbench_id = Uuidm.v4_gen (Random.get_state ()) ()} in
        let plan = Result.get ret in
        let expected =
          D.Plan.
            { workbench_id = id
            ; source = [Fun.(List.hd %> node_to_remain) nodes]
            ; dest = [Fun.(List.hd %> node_to_delete) nodes] }
        in
        Alcotest.(check (of_pp D.Plan.pp)) "plan" expected plan ;
        Lwt.return_unit ) ]

let test_set = move_plan_test_set @ delete_plan_test_set
