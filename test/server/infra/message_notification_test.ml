module D = Sxfiler_domain
module Tr = Sxfiler_server_infra.Message_notification

let id = Uuidm.v4_gen (Random.get_state ()) ()

let test_set =
  [ ( "can translate to/from domain"
    , `Quick
    , fun () ->
        let data = Tr.make ~id ~body:"foo" ~level:Tr.Info in
        Alcotest.(check @@ of_pp Fmt.nop) "domain" data Tr.Conv.(to_domain @@ of_domain data) )
  ; ( "can translate to/from yojson"
    , `Quick
    , fun () ->
        let data = Tr.make ~id ~body:"foo" ~level:Tr.Info |> Tr.Conv.of_domain in
        Alcotest.(check @@ result (of_pp Fmt.nop) (of_pp Fmt.nop))
          "yojson" (Ok data)
          Tr.Json.(of_json @@ to_json data) ) ]
