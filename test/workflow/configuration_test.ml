open Sxfiler_core
open Sxfiler_domain
module K = Sxfiler_kbd
module FL = Sxfiler_workflow

let test_set =
  let conf_t = Alcotest.testable Configuration_store.pp Configuration_store.equal in
  let event_t = Alcotest.testable FL.Configuration.pp_events FL.Configuration.equal_events in
  [
    Alcotest_lwt.test_case "update configuration entirely" `Quick (fun _ () ->
        let key = Configuration_store.Key.from_list [ "a" ] |> Option.get in
        let input = { FL.Configuration.Update.key; value = `String "foo" } in
        let expected = Configuration_store.put ~key ~value:(`String "foo") Configuration_store.empty in
        let load () = Lwt.return Configuration_store.empty in
        let save conf =
          Alcotest.(check conf_t) "conf" expected conf;
          Lwt.return_unit
        in
        let%lwt ret = FL.Configuration.update load save input in
        Alcotest.(check & list event_t) "command" [ FL.Configuration.Updated expected ] ret;
        Lwt.return_unit);
  ]
