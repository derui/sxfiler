module D = Sxfiler_domain
module T = Sxfiler_server_translator.Key_map

let data = Test_fixtures.Key_map.fixture [("j", "foo"); ("k", "bar")]

let test_set =
  [ ( "can translate to/from domain"
    , `Quick
    , fun () -> Alcotest.(check @@ of_pp Fmt.nop) "domain" data (T.to_domain @@ T.of_domain data)
    )
  ; ( "can translate to/from yojson"
    , `Quick
    , fun () ->
        let data = T.of_domain data in
        Alcotest.(check @@ result (of_pp T.pp) (of_pp Fmt.nop))
          "yojson" (Ok data)
          (T.of_json @@ T.to_json data) ) ]
