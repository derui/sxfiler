open Sxfiler_core
module D = Sxfiler_domain
module U = Sxfiler_usecase

let stat_base =
  D.File_stat.make ~mode:(Int32.of_int 0o775) ~uid:1000 ~gid:1000 ~atime:(Int64.of_int 100)
    ~mtime:(Int64.of_int 1000) ~ctime:(Int64.of_int 10000) ~size:Int64.max_int ~is_directory:false
    ~is_file:true ~is_symlink:true

let dir_stat =
  D.File_stat.make ~mode:(Int32.of_int 0o775) ~uid:1000 ~gid:1000 ~atime:(Int64.of_int 100)
    ~mtime:(Int64.of_int 1000) ~ctime:(Int64.of_int 10000) ~size:Int64.max_int ~is_directory:true
    ~is_file:false ~is_symlink:true

let node_base ~id ?(full_path = Path.of_string "foo") ?(stat = stat_base) ?(link_path = None) () =
  D.Node.make ~id ~full_path ~stat ~link_path

let test_set =
  [ Alcotest_lwt.test_case "move_nodes plan issue error when workbench not found" `Quick
      (fun _ () ->
         let module WR = struct
           let resolve _ = Lwt.return_none
           let store _ = assert false
           let remove _ = assert false
         end in
         let module Usecase = U.Plan.Filer.Move_nodes.Make (WR) in
         let%lwt result = Usecase.execute {workbench_id = Uuidm.v4_gen (Random.get_state ()) ()} in
         Alcotest.(check @@ result (of_pp Fmt.nop) (of_pp Fmt.nop))
           "not found workbench"
           (Error `Not_found_wb)
           result ;
         Lwt.return_unit )
  ; Alcotest_lwt.test_case "get the plan to move nodes" `Quick (fun _ () ->
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
            ~location:Path.(of_string "/foo")
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
        match%lwt Usecase.execute {workbench_id = Uuidm.v4_gen (Random.get_state ()) ()} with
        | Error _ -> Alcotest.fail "Unknown error"
        | Ok plan ->
          let expected =
            D.Plan.
              { workbench_id = id
              ; source = [Fun.(List.hd %> node_to_delete) nodes]
              ; dest = [Fun.(List.hd %> node_to_append) nodes] }
          in
          Alcotest.(check (of_pp Fmt.nop)) "plan" expected plan ;
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
            Alcotest.(check (of_pp Fmt.nop)) "plan" expected plan ;
            Lwt.return_unit ) ]
