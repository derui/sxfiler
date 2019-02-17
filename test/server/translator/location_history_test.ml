module D = Sxfiler_domain
module Tr = Sxfiler_server_translator

let data =
  Test_fixtures.Location_history.fixture
    [Test_fixtures.Location_record.fixture ~location:"/foo" ~timestamp:0L]

let test_set =
  [ ( "can translate to/from domain"
    , `Quick
    , fun () ->
      Alcotest.(check @@ of_pp D.Location_history.pp)
        "domain" data
        Tr.Location_history.(to_domain @@ of_domain data) )
  ; ( "can translate to/from yojson"
    , `Quick
    , fun () ->
      let data = Tr.Location_history.of_domain data in
      Alcotest.(check @@ result (of_pp Tr.Location_history.pp) (of_pp Fmt.nop))
        "yojson" (Ok data)
        Tr.Location_history.(of_yojson @@ to_yojson data) ) ]
