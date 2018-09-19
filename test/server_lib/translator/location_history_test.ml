open Sxfiler_core
module D = Sxfiler_domain
module Tr = Sxfiler_server_translator
module T = Sxfiler_rpc.Types

let data =
  let data' = D.Location_history.make () in
  let record = {D.Location_record.location = Path.of_string " /foo"; timestamp = Int64.max_int} in
  D.Location_history.add_record ~record data'

let testcases =
  [ ( "can translate to/from domain"
    , `Quick
    , fun () ->
      let expected =
        { T.Location_history.records = List.map Tr.Location_record.of_domain data.records
        ; max_records = data.max_records }
      in
      Alcotest.(check @@ of_pp Fmt.nop) "domain" expected (Tr.Location_history.of_domain data) )
  ; ( "can translate to/from yojson"
    , `Quick
    , fun () ->
      let data = Tr.Location_history.of_domain data in
      Alcotest.(check @@ result (of_pp Fmt.nop) (of_pp Fmt.nop))
        "yojson" (Ok data)
        (Tr.Location_history.of_yojson @@ Tr.Location_history.to_yojson data) ) ]

let () = Alcotest.run "location history translator" [("translation", testcases)]
