open Sxfiler_core
module D = Sxfiler_domain
module Tr = Sxfiler_server_translator.Location_record
module T = Sxfiler_rpc.Types

let data = {D.Location_record.location = Path.of_string "/foo"; timestamp = Int64.max_int}

let testcases =
  [ ( "can translate to/from domain"
    , `Quick
    , fun () ->
      let expected =
        {T.Location_record.location = "/foo"; timestamp = Int64.to_string Int64.max_int}
      in
      Alcotest.(check @@ of_pp Fmt.nop) "domain" expected (Tr.of_domain data) )
  ; ( "can translate to/from yojson"
    , `Quick
    , fun () ->
      let data = Tr.of_domain data in
      Alcotest.(check @@ result (of_pp Fmt.nop) (of_pp Fmt.nop))
        "yojson" (Ok data)
        (Tr.of_yojson @@ Tr.to_yojson data) ) ]

let () = Alcotest.run "location record translator" [("translation", testcases)]
