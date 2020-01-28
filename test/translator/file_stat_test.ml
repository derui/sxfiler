module D = Sxfiler_domain
module T = Sxfiler_translator.File_stat

let data = Test_fixtures.File_stat.file_fixture ()

let test_set =
  [
    Alcotest_lwt.test_case_sync "can translate to/from domain" `Quick (fun () ->
        Alcotest.(check @@ result (of_pp D.File_stat.pp) @@ of_pp T.pp_error)
          "domain" (Ok data)
          (T.to_domain @@ T.of_domain data));
  ]
