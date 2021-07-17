open Sxfiler_domain
module S = Sxfiler_dependency
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
  let get_mock expected =
    (module struct
      let provide_collection () = Lwt.return expected

      let update_collection _ = failwith "not implemented"
    end : FL.Common_step.Completer.Instance)
  in

  [
    ( Alcotest_lwt.test_case "call read API for completer" `Quick @@ fun _ () ->
      let module V = (val get_mock expected_collection) in
      let%lwt items =
        FL.Common_step.Completer.read "input"
        |> S.provide (function
             | `Step_completer_instance c -> S.Context.value (module V : FL.Common_step.Completer.Instance) c
             | `Completer_instance c      -> S.Context.value (module I : Completer.Instance) c)
        |> S.run
      in
      Alcotest.(check @@ list F.Testable.Completer.canditate) "items" [] items;
      Lwt.return_unit );
  ]
