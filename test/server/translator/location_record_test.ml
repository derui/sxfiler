module D = Sxfiler_domain
module Tr = Sxfiler_server_translator.Location_record

let data = Test_fixtures.Location_record.fixture ~location:"/foo" ~timestamp:1L

let test_set =
  [
    ( "can translate to/from domain",
      `Quick,
      fun () ->
        Alcotest.(check @@ of_pp D.Location_record.pp)
          "domain" data
          Tr.(data |> of_domain |> to_domain) );
  ]
