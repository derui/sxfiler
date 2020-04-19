module D = Sxfiler_domain
module T = Sxfiler_translator.Context

let test_set =
  [
    Alcotest_lwt.test_case_sync "can translate to/from domain" `Quick (fun () ->
        let data = D.Context.of_list [ "completing" ] in
        Alcotest.(check @@ result (of_pp Fmt.nop) @@ of_pp Fmt.nop) "domain" (Ok data) (T.to_domain @@ T.of_domain data));
  ]
