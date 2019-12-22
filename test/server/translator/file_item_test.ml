module D = Sxfiler_domain
module Tr = Sxfiler_server_translator.File_item

let stat = Test_fixtures.File_stat.fixture ()
let data = Test_fixtures.File_item.fixture stat

let test_set =
  [
    ( "can translate to/from domain",
      `Quick,
      fun () ->
        Alcotest.(check Test_fixtures.Testable.file_item)
          "domain" data
          (data |> Tr.of_domain |> Tr.to_domain) );
  ]
