open Sxfiler_core
module D = Sxfiler_domain
module Tr = Sxfiler_server_translator
module T = Sxfiler_rpc.Types

let data =
  { D.Filer.id = "id"
  ; location = Path.of_string "/bar"
  ; nodes = []
  ; sort_order = D.Types.Sort_type.Date
  ; history = D.Location_history.make () }

let testcases =
  [ ( "can translate to/from domain"
    , `Quick
    , fun () ->
      let expected =
        { T.Filer.id = "id"
        ; location = "/bar"
        ; nodes = []
        ; history = Tr.Location_history.of_domain @@ D.Location_history.make () }
      in
      Alcotest.(check @@ of_pp Fmt.nop) "domain" expected (Tr.Filer.of_domain data) )
  ; ( "can translate to/from yojson"
    , `Quick
    , fun () ->
      let data = Tr.Filer.of_domain data in
      Alcotest.(check @@ result (of_pp Fmt.nop) (of_pp Fmt.nop))
        "yojson" (Ok data)
        (Tr.Filer.of_yojson @@ Tr.Filer.to_yojson data) ) ]

let () = Alcotest.run "filer translator" [("translation", testcases)]
