module D = Sxfiler_domain
module Tr = Sxfiler_server_translator.Notification

let test_set =
  [ ( "can translate to/from domain"
    , `Quick
    , fun () ->
      let data = Test_fixtures.Notification.for_message "foo" in
      Alcotest.(check @@ of_pp Fmt.nop) "domain" data Tr.(to_domain @@ of_domain data) )
  ; ( "can translate to/from yojson"
    , `Quick
    , fun () ->
      let data =
        Test_fixtures.Notification.for_progress ~process:"foo" ~current:1. ~targeted:10. ()
        |> Tr.of_domain
      in
      Alcotest.(check @@ result (of_pp Tr.pp) (of_pp Fmt.nop))
        "yojson" (Ok data)
        Tr.(of_yojson @@ to_yojson data) ) ]
