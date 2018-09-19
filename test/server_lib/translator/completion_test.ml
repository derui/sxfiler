open Sxfiler_core
module D = Sxfiler_completion.Domain
module Tr = Sxfiler_server_translator.Completion
module T = Sxfiler_rpc.Types

let testcases =
  [ ( "can translate item to/from domain"
    , `Quick
    , fun () ->
      let data = {D.Item.id = "value"; value = "foobar"} in
      Alcotest.(check @@ of_pp Fmt.nop) "empty" data (Tr.Item.to_domain @@ Tr.Item.of_domain data)
    )
  ; ( "can translate candidate to/from domain"
    , `Quick
    , fun () ->
      let data =
        {D.Candidate.start = 1; length = 20; value = {D.Item.id = "foo"; value = "value"}}
      in
      Alcotest.(check @@ of_pp Fmt.nop) "empty" data Tr.Candidate.(to_domain @@ of_domain data)
    )
  ; ( "can translate item to/from json"
    , `Quick
    , fun () ->
      let data = {T.Completion.Item.id = "value"; value = "foobar"} in
      Alcotest.(check @@ result (of_pp Fmt.nop) string)
        "yojson" (Ok data)
        Tr.Item.(of_yojson @@ to_yojson data) )
  ; ( "can translate candidate to/from json"
    , `Quick
    , fun () ->
      let data =
        { T.Completion.Candidate.start = 1
        ; length = 20
        ; value = {T.Completion.Item.id = "foo"; value = "value"} }
      in
      Alcotest.(check @@ result (of_pp Fmt.nop) string)
        "yojson" (Ok data)
        Tr.Candidate.(of_yojson @@ to_yojson data) ) ]

let () = Alcotest.run "completion translator" [("translation", testcases)]
