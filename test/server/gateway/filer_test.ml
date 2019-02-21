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

let test_set =
  [ Alcotest_lwt.test_case "create new filer if it does not exists" `Quick (fun _ () ->
        let file_tree =
          D.File_tree.make ~location:(Path.of_string ~env:`Unix "/initial") ~nodes:[]
        in
        let expected =
          Test_fixtures.Filer.fixture "foo" ~file_tree ~sort_order:D.Types.Sort_type.Date
        in
        let module Usecase = struct
          include U.Filer.Make.Type

          let execute _ = Lwt.return_ok expected
        end in
        let module Gateway = G.Filer.Make.Make (Dummy_system) (Usecase) in
        let%lwt res = Gateway.handle {Gateway.initial_location = "/initial"; name = "foo"} in
        Alcotest.(check @@ of_pp T.Filer.pp) "created" (T.Filer.of_domain expected) res ;
        Lwt.return_unit )
  ; Alcotest_lwt.test_case "do not create workspace if it exists" `Quick (fun _ () ->
        let module Usecase = struct
          include U.Filer.Make.Type

          let execute _ = Lwt.return_error `Already_exists
        end in
        let module Gateway = G.Filer.Make.Make (Dummy_system) (Usecase) in
        try%lwt
          let%lwt _ = Gateway.handle {initial_location = "/initial"; name = "foo"} in
          Alcotest.fail "not thrown any exception" |> Lwt.return
        with
        | G.Gateway_error.(Gateway_error Filer_already_exists) -> Lwt.return_unit
        | _ -> Alcotest.fail "catch different exception" )
  ; Alcotest_lwt.test_case "move_parent raise error if filer is not found" `Quick (fun _ () ->
        let module Usecase = struct
          include U.Filer.Move_parent.Type

          let execute _ = Lwt.return_error `Not_found
        end in
        let module Gateway = G.Filer.Move_parent.Make (Usecase) in
        try%lwt
          let%lwt _ = Gateway.handle {name = "foo"} in
          Alcotest.fail "not thrown any exception" |> Lwt.return
        with
        | G.Gateway_error.(Gateway_error Filer_not_found) -> Lwt.return_unit
        | _ -> Alcotest.fail "catch different exception" ) ]
