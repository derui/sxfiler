open Sxfiler_core
module F = Test_fixtures
module D = Sxfiler_domain
module U = Sxfiler_usecase

let delete_nodes_test_set =
  [ Alcotest_lwt.test_case "issue error if workbench not found" `Quick (fun _ () ->
        let filer =
          D.Filer.make ~id:"foo"
            ~location:Path.(of_string "/bar")
            ~nodes:[F.Node.fixture F.File_stat.(fixture ())]
            ~history:D.Location_history.(make ())
            ~sort_order:D.Types.Sort_type.Name
        in
        let wb_id = Uuidm.v4_gen (Random.get_state ()) () in
        let module FR = struct
          let data = ref filer
          let resolve _ = assert false

          let store v =
            data := v ;
            Lwt.return_unit
        end in
        let module WR : D.Workbench.Repository = struct
          let resolve _ = Lwt.return_none
          let store _ = assert false
          let remove _ = assert false
        end in
        let module Svc = struct
          type location = Path.t

          let _, f = Spy.wrap (fun _ -> Lwt.return_unit)
          let trash = f
        end in
        let module Scan = struct
          let scan _ = assert false
        end in
        let module E = U.Filer.Delete_nodes.Make (FR) (WR) (Scan) (Svc) in
        let%lwt result = E.execute {workbench_id = Uuidm.to_string wb_id} in
        Alcotest.(check @@ result unit (of_pp Fmt.nop))
          "not found wb"
          (Error `Not_found_workbench)
          result ;
        Lwt.return_unit )
  ; Alcotest_lwt.test_case "delete source nodes if workbench exists" `Quick (fun _ () ->
        let filer =
          D.Filer.make ~id:"foo"
            ~location:Path.(of_string "/bar")
            ~nodes:[F.Node.fixture F.File_stat.(fixture ())]
            ~history:D.Location_history.(make ())
            ~sort_order:D.Types.Sort_type.Name
        in
        let wb_id = Uuidm.v4_gen (Random.get_state ()) () in
        let wb_env =
          D.Workbench.
            {source = filer; dest = filer; nodes = [node_base ~id:"bar" ~stat:file_stat ()]}
        in
        let wb = D.Workbench.make ~id:wb_id ~env:wb_env ~corrections:[] in
        let module FR = struct
          let resolve _ = assert false
          let spy, store = Spy.wrap (fun _ -> Lwt.return_unit)
        end in
        let module WR : D.Workbench.Repository = struct
          let resolve _ = Lwt.return_some wb
          let store _ = assert false
          let remove _ = assert false
        end in
        let module Svc = struct
          type location = Path.t

          let spy, trash = Spy.wrap (fun _ -> Lwt.return_unit)
        end in
        let module Scan = struct
          let scan _ = Lwt.return []
        end in
        let module E = U.Filer.Delete_nodes.Make (FR) (WR) (Scan) (Svc) in
        let%lwt result = E.execute {workbench_id = Uuidm.to_string wb_id} in
        Alcotest.(check @@ result unit (of_pp Fmt.nop)) "delete nodes" (Ok ()) result ;
        Alcotest.(check @@ list @@ of_pp D.Filer.pp)
          "new filer" (Spy.Wrap.called_args FR.spy)
          [D.Filer.{filer with nodes = []}] ;
        Alcotest.(check @@ list @@ list @@ of_pp D.Node.pp)
          "called" (Spy.Wrap.called_args Svc.spy) [wb_env.nodes] ;
        Lwt.return_unit ) ]

let test_set =
  delete_nodes_test_set
  @ [ Alcotest_lwt.test_case "renewal filer with entered directory" `Quick (fun _ () ->
      let filer =
        D.Filer.make ~id:"foo"
          ~location:Path.(of_string "/bar")
          ~nodes:[node_base ~id:"bar" ~stat:dir_stat ()]
          ~history:D.Location_history.(make ())
          ~sort_order:D.Types.Sort_type.Name
      in
      let module SR = struct
        let data = ref filer
        let resolve _ = Lwt.return_some filer

        let store v =
          data := v ;
          Lwt.return_unit
      end in
      let new_nodes = [node_base ~id:"foo" ()] in
      let module Svc = struct
        let scan _ = Lwt.return new_nodes
      end in
      let module Clock = struct
        let unixtime () = Int64.min_int
      end in
      let module Usecase = U.Filer.Enter_directory.Make (SR) (Svc) (Clock) in
      let%lwt result = Usecase.execute {name = "foo"; node_id = "bar"} in
      Alcotest.(check @@ result (of_pp Fmt.nop) (of_pp Fmt.nop))
        "renew filer" (Ok !SR.data) result ;
      Alcotest.(check @@ result (of_pp Fmt.nop) (of_pp Fmt.nop))
        "renew location"
        (Ok Path.(of_string "foo"))
        Result.Infix.(result >|= fun v -> v.location) ;
      Alcotest.(check @@ result (of_pp Fmt.nop) (of_pp Fmt.nop))
        "renew nodes" (Ok new_nodes)
        Result.Infix.(result >|= fun v -> v.nodes) ;
      Lwt.return_unit )
    ; Alcotest_lwt.test_case "error when filer not found" `Quick (fun _ () ->
          let module SR = struct
            let resolve _ = Lwt.return_none
            let store _ = assert false
          end in
          let module Svc = struct
            let scan _ = assert false
          end in
          let module Clock = struct
            let unixtime () = Int64.min_int
          end in
          let module Usecase = U.Filer.Enter_directory.Make (SR) (Svc) (Clock) in
          let%lwt result = Usecase.execute {name = "foo"; node_id = "bar"} in
          Alcotest.(check @@ result (of_pp Fmt.nop) (of_pp Fmt.nop))
            "not found filer"
            (Error `Not_found_filer)
            result ;
          Lwt.return_unit )
    ; Alcotest_lwt.test_case "issue error when node not found" `Quick (fun _ () ->
          let filer =
            D.Filer.make ~id:"foo"
              ~location:Path.(of_string "/bar")
              ~nodes:[node_base ~id:"bar" ~stat:dir_stat ()]
              ~history:D.Location_history.(make ())
              ~sort_order:D.Types.Sort_type.Name
          in
          let module SR = struct
            let resolve _ = Lwt.return_some filer
            let store _ = assert false
          end in
          let module Svc = struct
            let scan _ = assert false
          end in
          let module Clock = struct
            let unixtime () = Int64.min_int
          end in
          let module Usecase = U.Filer.Enter_directory.Make (SR) (Svc) (Clock) in
          let%lwt result = Usecase.execute {name = "foo"; node_id = "not found"} in
          Alcotest.(check @@ result (of_pp Fmt.nop) (of_pp Fmt.nop))
            "not found node"
            (Error `Not_found_node)
            result ;
          Lwt.return_unit ) ]
