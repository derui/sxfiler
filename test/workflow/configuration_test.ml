open Sxfiler_core
open Sxfiler_domain
module F = Test_fixtures.Testable
module K = Sxfiler_kbd
module FL = Sxfiler_workflow

let test_set =
  [
    Alcotest_lwt.test_case "update configuration entirely" `Quick (fun _ () ->
        let input = Configuration.(default_sort_order Types.Sort_type.Size default) in
        let save_configuration conf =
          Alcotest.(check & F.configuration) "conf" input conf;
          Lwt.return_unit
        in
        let%lwt ret = FL.Configuration.update save_configuration input in
        let events = Alcotest.testable FL.Configuration.pp_events FL.Configuration.equal_events in
        Alcotest.(check & list events) "command" [ FL.Configuration.Updated input ] ret;
        Lwt.return_unit);
  ]
