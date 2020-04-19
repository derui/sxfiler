module D = Sxfiler_domain
module Tr = Sxfiler_translator.File_item

let stat = Test_fixtures.File_stat.file_fixture ()

let data = Test_fixtures.File_item.fixture ~stat ()

let test_set =
  [
    Alcotest_lwt.test_case_sync "can translate to/from domain" `Quick (fun () ->
        Alcotest.(check @@ result Test_fixtures.Testable.file_item @@ of_pp Fmt.nop)
          "domain" (Ok data)
          (data |> Tr.of_domain |> Tr.to_domain));
  ]
