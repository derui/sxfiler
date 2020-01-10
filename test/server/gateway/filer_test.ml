open Sxfiler_core
module D = Sxfiler_domain
module U = Sxfiler_usecase
module G = Sxfiler_server_gateway
module T = Sxfiler_server_translator
module Gen = Sxfiler_server_generated

module Factory = D.Filer.Factory.Make (struct
  type id = D.Filer.id

  let state = Random.get_state ()
  let generate () = Uuidm.v4_gen state ()
end)

module Dummy_system = struct
  let getcwd () = "/foo"
end

let test_set =
  [
    Alcotest_lwt.test_case "create new filer if it does not exists" `Quick (fun _ () ->
        let file_list =
          D.File_list.make ~location:(Path.of_string ~env:`Unix "/initial") ~items:[] ()
        in
        let expected = Factory.create ~name:"foo" ~file_list ~sort_order:D.Types.Sort_type.Date in
        let module Usecase = struct
          include U.Filer.Make.Type

          let execute _ = Lwt.return_ok expected
        end in
        let module Gateway = G.Filer.Make.Make (Dummy_system) (Usecase) in
        let%lwt res = Gateway.handle { initialLocation = "/initial"; name = "foo" } in
        Alcotest.(check @@ result (of_pp Fmt.nop) (of_pp Fmt.nop))
          "created"
          (Ok Gen.Filer.FilerMakeResponse.{ filer = Some (T.Filer.of_domain expected) })
          res;
        Lwt.return_unit);
    Alcotest_lwt.test_case "jump a location for filer" `Quick (fun _ () ->
        let file_list =
          D.File_list.make ~location:(Path.of_string ~env:`Unix "/initial") ~items:[] ()
        in
        let expected = Factory.create ~name:"foo" ~file_list ~sort_order:D.Types.Sort_type.Date in
        let module Usecase = struct
          include U.Filer.Jump_location.Type

          let execute _ = Lwt.return_ok expected
        end in
        let module Gateway = G.Filer.Jump_location.Make (Dummy_system) (Usecase) in
        let%lwt res = Gateway.handle { location = "/initial"; name = "foo" } in
        Alcotest.(check @@ result (of_pp Fmt.nop) (of_pp Fmt.nop))
          "created"
          (Ok { Gen.Filer.FilerJumpLocationResponse.filer = Some (T.Filer.of_domain expected) })
          res;
        Lwt.return_unit);
    Alcotest_lwt.test_case "do not create workspace if it exists" `Quick (fun _ () ->
        let module Usecase = struct
          include U.Filer.Make.Type

          let execute _ = Lwt.return_error `Already_exists
        end in
        let module Gateway = G.Filer.Make.Make (Dummy_system) (Usecase) in
        let%lwt ret = Gateway.handle { initialLocation = "/initial"; name = "foo" } in
        Alcotest.(check @@ result (of_pp Fmt.nop) (of_pp Fmt.nop))
          "thrown any exception" (Error G.Gateway_error.Filer_already_exists) ret;
        Lwt.return_unit);
    Alcotest_lwt.test_case "move_parent raise error if filer is not found" `Quick (fun _ () ->
        let module Usecase = struct
          include U.Filer.Move_parent.Type

          let execute _ = Lwt.return_error `Not_found
        end in
        let module Gateway = G.Filer.Move_parent.Make (Usecase) in
        let%lwt ret = Gateway.handle { name = "foo" } in
        Alcotest.(check @@ result (of_pp Fmt.nop) (of_pp Fmt.nop))
          "thrown any exception" (Error G.Gateway_error.Filer_not_found) ret;
        Lwt.return_unit);
    Alcotest_lwt.test_case "toggle_mark raise error if filer is not found" `Quick (fun _ () ->
        let module Usecase = struct
          include U.Filer.Toggle_mark.Type

          let execute _ = Lwt.return_error `Not_found
        end in
        let module Gateway = G.Filer.Toggle_mark.Make (Usecase) in
        let%lwt ret = Gateway.handle { name = "foo"; itemIds = [ "id1" ] } in
        Alcotest.(check @@ result (of_pp Fmt.nop) (of_pp Fmt.nop))
          "thrown any exception" (Error G.Gateway_error.Filer_not_found) ret;
        Lwt.return_unit);
    Alcotest_lwt.test_case "move raise error if filer is not found" `Quick (fun _ () ->
        let module Usecase = struct
          include U.Filer.Move.Type

          let execute _ = Lwt.return_error (`Not_found "foo")
        end in
        let module Gateway = G.Filer.Move.Make (Usecase) in
        let%lwt ret = Gateway.handle { source = "foo"; dest = "dest"; itemIds = [ "id1" ] } in
        Alcotest.(check @@ result (of_pp Fmt.nop) (of_pp Fmt.nop))
          "thrown any exception" (Error G.Gateway_error.Filer_not_found) ret;
        Lwt.return_unit);
    Alcotest_lwt.test_case "move gateway raise error if same filer" `Quick (fun _ () ->
        let module Usecase = struct
          include U.Filer.Move.Type

          let execute _ = Lwt.return_error `Same_filer
        end in
        let module Gateway = G.Filer.Move.Make (Usecase) in
        let%lwt ret = Gateway.handle { source = "foo"; dest = "dest"; itemIds = [ "id1" ] } in
        Alcotest.(check @@ result (of_pp Fmt.nop) (of_pp Fmt.nop))
          "thrown any exception" (Error G.Gateway_error.Filer_same_filer) ret;
        Lwt.return_unit);
    Alcotest_lwt.test_case "delete gateway raise error if filer is not found" `Quick (fun _ () ->
        let module Usecase = struct
          include U.Filer.Delete.Type

          let execute _ = Lwt.return_error (`Not_found "foo")
        end in
        let module Gateway = G.Filer.Delete.Make (Usecase) in
        let%lwt ret = Gateway.handle { source = "foo"; itemIds = [ "id1" ] } in
        Alcotest.(check @@ result (of_pp Fmt.nop) (of_pp Fmt.nop))
          "thrown any exception" (Error G.Gateway_error.Filer_not_found) ret;
        Lwt.return_unit);
    Alcotest_lwt.test_case "copy gateway raise error if filer is not found" `Quick (fun _ () ->
        let module Usecase = struct
          include U.Filer.Copy.Type

          let execute _ = Lwt.return_error (`Not_found "foo")
        end in
        let module Gateway = G.Filer.Copy.Make (Usecase) in
        let%lwt ret = Gateway.handle { source = "foo"; dest = "dest"; itemIds = [ "id1" ] } in
        Alcotest.(check @@ result (of_pp Fmt.nop) (of_pp Fmt.nop))
          "thrown any exception" (Error G.Gateway_error.Filer_not_found) ret;
        Lwt.return_unit);
  ]
