module D = Sxfiler_domain
module T = Sxfiler_server_translator.Key_map

let data = Test_fixtures.Key_map.fixture [ ("j", "foo"); ("k", "bar") ]

let test_set =
  [
    ( "can translate to/from domain",
      `Quick,
      fun () -> Alcotest.(check @@ of_pp Fmt.nop) "domain" data (T.to_domain @@ T.of_domain data) );
  ]
