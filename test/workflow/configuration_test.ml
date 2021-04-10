open Sxfiler_core
open Sxfiler_domain
module FL = Sxfiler_workflow
module S = Sxfiler_dependency

let test_set =
  let conf_t = Alcotest.testable Configuration_store.pp Configuration_store.equal in
  let event_t = Alcotest.testable FL.Configuration.pp_event FL.Configuration.equal_event in
  [
    Alcotest_lwt.test_case "update configuration entirely" `Quick (fun _ () ->
        let key = Configuration_store.Key.from_list [ "a" ] |> Option.get in
        let input = { FL.Configuration.Update.key; value = `String "foo" } in
        let expected = Configuration_store.put ~key ~value:(`String "foo") Configuration_store.empty in
        let mock =
          ( module struct
            let load () = Lwt.return Configuration_store.empty

            let save conf =
              Alcotest.(check conf_t) "conf" expected conf;
              Lwt.return_unit
          end : FL.Common_step.Configuration.Instance )
        in
        let%lwt ret =
          FL.Configuration.update input
          |> S.provide (function `Step_configuration_instance c -> S.Context.value mock c)
          |> S.run
        in
        Alcotest.(check & list event_t) "command" [ FL.Configuration.Updated expected ] ret;
        Lwt.return_unit);
  ]
