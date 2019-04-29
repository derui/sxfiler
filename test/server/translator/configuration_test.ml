open Sxfiler_core
module D = Sxfiler_domain
module T = Sxfiler_server_translator.Configuration

let test_set =
  [ ( "can translate to/from domain"
    , `Quick
    , fun () ->
        let data = D.Configuration.default in
        Alcotest.(check @@ of_pp Fmt.nop) "domain" data (T.to_domain @@ T.of_domain data) )
  ; ( "can translate to/from yojson"
    , `Quick
    , fun () ->
        let data = T.of_domain @@ D.Configuration.default in
        Alcotest.(check @@ result (of_pp Fmt.nop) (of_pp Fmt.nop))
          "yojson" (Ok data)
          (Fun.(T.to_json %> T.of_json) data) ) ]
