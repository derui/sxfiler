module D = Sxfiler_domain
module Tr = Sxfiler_server_infra.Progress_notification

let test_set =
  let id = Uuidm.v4_gen (Random.get_state ()) () in
  let data = Tr.make ~id ~body:{process = "foo"; current = 1.0; targeted = 100.0} in
  [ ( "can translate to/from domain"
    , `Quick
    , fun () ->
        Alcotest.(check @@ of_pp Fmt.nop) "domain" data Tr.Conv.(to_domain @@ of_domain data) )
  ; ( "can translate to/from yojson"
    , `Quick
    , fun () ->
        let data = Tr.Conv.of_domain data in
        Alcotest.(check @@ result (of_pp Fmt.nop) (of_pp Fmt.nop))
          "yojson" (Ok data)
          Tr.Json.(of_json @@ to_json data) ) ]
