module D = Sxfiler_domain
module Tr = Sxfiler_server_translator.File_item

let stat = Test_fixtures.File_stat.fixture ()
let data = Test_fixtures.File_item.fixture stat

let test_set =
  [ ( "can translate to/from domain"
    , `Quick
    , fun () ->
      Alcotest.(check Test_fixtures.Testable.file_item)
        "domain" data
        (data |> Tr.of_domain |> Tr.to_domain) )
  ; ( "can translate to/from yojson"
    , `Quick
    , fun () ->
      let data = Tr.of_domain data in
      Alcotest.(check @@ result (of_pp Tr.pp) (of_pp Fmt.nop))
        "yojson" (Ok data)
        Tr.(data |> to_json |> of_json) ) ]
