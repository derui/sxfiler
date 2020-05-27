module D = Sxfiler_domain
module T = Sxfiler_translator.Configuration_store
module G = Sxfiler_generated.Configuration

let test_set =
  let value_t = Alcotest.testable G.Configuration.pp G.Configuration.equal in
  [
    Alcotest_lwt.test_case_sync "can translate from domain" `Quick (fun () ->
        let data = D.Configuration_store.empty in
        Alcotest.(check @@ list value_t) "domain" [] (T.of_domain data));
  ]
