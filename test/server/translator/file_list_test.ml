open Sxfiler_core
module D = Sxfiler_domain
module Tr = Sxfiler_server_translator

let data = Test_fixtures.(File_list.empty_list (Path.of_string "/bar"))

let test_set =
  [ ( "can translate to/from domain"
    , `Quick
    , fun () ->
      Alcotest.(check @@ of_pp D.File_list.pp)
        "domain" data
        Tr.File_list.(to_domain @@ of_domain data) )
  ; ( "can translate to/from yojson"
    , `Quick
    , fun () ->
      let data = Tr.File_list.of_domain data in
      Alcotest.(check @@ result (of_pp Tr.File_list.pp) (of_pp Fmt.nop))
        "yojson" (Ok data)
        Tr.File_list.(of_json @@ to_json data) ) ]
