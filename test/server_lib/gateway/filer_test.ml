open Sxfiler_core
module D = Sxfiler_domain
module S = Sxfiler_server
module U = Sxfiler_usecase
module C = Sxfiler_server_core
module G = Sxfiler_server_gateway
module T = Sxfiler_server_translator

module Dummy_system = struct
  let getcwd () = "/foo"
end

let filer_tests =
  [ Alcotest_lwt.test_case "create new filer if it does not exists" `Quick (fun switch () ->
        let expected =
          D.Filer.make ~id:"foo"
            ~location:(Path.of_string ~env:`Unix "/initial")
            ~nodes:[] ~sort_order:D.Types.Sort_type.Date
            ~history:D.Location_history.(make ())
        in
        let module Usecase = struct
          include U.Filer.Make_type

          let execute _ = Lwt.return_ok expected
        end in
        let module Gateway = G.Filer.Make.Make (Dummy_system) (Usecase) in
        let%lwt res = Gateway.handle {Gateway.initial_location = "/initial"; name = "foo"} in
        Alcotest.(check @@ option @@ of_pp @@ Fmt.nop)
          "created"
          (Option.some @@ T.Filer.of_domain expected)
          res.filer ;
        Lwt.return_unit )
  ; Alcotest_lwt.test_case "do not create workspace if it exists" `Quick (fun switch () ->
        let module Usecase = struct
          include U.Filer.Make_type

          let execute _ = Lwt.return_error `Already_exists
        end in
        let module Gateway = G.Filer.Make.Make (Dummy_system) (Usecase) in
        let%lwt res = Gateway.handle {initial_location = "/initial"; name = "foo"} in
        Alcotest.(check bool) "error" true res.already_exists ;
        Lwt.return_unit )
  ; Alcotest_lwt.test_case "move_parent raise error if filer is not found" `Quick (fun switch () ->
        let module Usecase = struct
          include U.Filer.Move_parent_type

          let execute _ = Lwt.return_error `Not_found
        end in
        let module Gateway = G.Filer.Move_parent.Make (Usecase) in
        let%lwt res = Gateway.handle {name = "foo"} in
        Alcotest.(check bool) "error" true res.not_found ;
        Lwt.return_unit )
  ; Alcotest_lwt.test_case "make plan to move nodes" `Quick (fun switch () ->
        let id = Uuidm.v4_gen (Random.get_state ()) () in
        let plan = D.Plan.make ~workbench_id:id ~source:[] ~dest:[] in
        let filer =
          D.Filer.make ~id:"foo"
            ~location:(Path.of_string ~env:`Unix "/initial")
            ~nodes:[] ~sort_order:D.Types.Sort_type.Date
            ~history:D.Location_history.(make ())
        in
        let module Usecase = struct
          include U.Filer.Plan_move_nodes.Type

          let execute {workbench_id} =
            Alcotest.(check @@ of_pp Fmt.nop) "id" id workbench_id ;
            Lwt.return_ok plan
        end in
        let module Wb = struct
          include U.Workbench.Make_type

          let execute {from; node_ids; _to} =
            Alcotest.(check string) "from" "from" from ;
            Alcotest.(check @@ list string) "ids" ["id"] node_ids ;
            Alcotest.(check string) "to" "to" _to ;
            Lwt.return_ok
            @@ D.Workbench.make ~id ~env:{D.Workbench.source = filer; nodes = []; dest = filer}
        end in
        let module Gateway = G.Filer.Plan_move_nodes.Make (Wb) (Usecase) in
        let%lwt res = Gateway.handle {from = "from"; node_ids = ["id"]; _to = "to"} in
        Alcotest.(check @@ option @@ of_pp @@ Fmt.nop)
          "created"
          (Option.some @@ T.Plan.of_domain plan)
          res.plan ;
        Lwt.return_unit ) ]

let testcases = [("rpc procedure : filer", filer_tests)]
