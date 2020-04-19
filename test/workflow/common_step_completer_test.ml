open Sxfiler_domain
module F = Test_fixtures
module FL = Sxfiler_workflow

let test_set =
  let expected_collection = [ Completer.Item.make ~id:"id" ~value:"The item" ] in
  let module I = struct
    module Completer = struct
      type t = unit

      let read ~input ~collection () =
        Alcotest.(check string) "input" "input" input;
        Alcotest.(check @@ list F.Testable.Completer.item) "collection" expected_collection collection;
        []
    end

    let this = ()
  end in
  [
    ( Alcotest_lwt.test_case "call read API for completer" `Quick @@ fun _ () ->
      let open Completer in
      let provide_collection () = Lwt.return expected_collection in
      let%lwt items = FL.Common_step.Completer.read provide_collection (module I : Instance) "input" in
      Alcotest.(check @@ list F.Testable.Completer.canditate) "items" [] items;
      Lwt.return_unit );
  ]
