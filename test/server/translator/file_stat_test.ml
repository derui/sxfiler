module D = Sxfiler_domain
module T = Sxfiler_server_translator.File_stat

let data = Test_fixtures.File_stat.fixture ~directory:true ()

let test_set =
  [ ( "can translate to/from domain"
    , `Quick
    , fun () ->
        Alcotest.(check @@ of_pp D.File_stat.pp) "domain" data (T.to_domain @@ T.of_domain data) )
  ; ( "can translate to/from yojson"
    , `Quick
    , fun () ->
        let data = T.of_domain data in
        Alcotest.(check @@ result (of_pp T.pp) (of_pp Fmt.nop))
          "yojson" (Ok data)
          (T.of_json @@ T.to_json data) ) ]
