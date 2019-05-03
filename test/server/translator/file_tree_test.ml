open Sxfiler_core
module D = Sxfiler_domain
module Tr = Sxfiler_server_translator

let data = Test_fixtures.(File_tree.empty_tree (Path.of_string "/bar"))

let test_set =
  [ ( "can translate to/from domain"
    , `Quick
    , fun () ->
      Alcotest.(check @@ of_pp D.File_tree.pp)
        "domain" data
        Tr.File_tree.(to_domain @@ of_domain data) )
  ; ( "can translate to/from yojson"
    , `Quick
    , fun () ->
      let data = Tr.File_tree.of_domain data in
      Alcotest.(check @@ result (of_pp Tr.File_tree.pp) (of_pp Fmt.nop))
        "yojson" (Ok data)
        Tr.File_tree.(of_json @@ to_json data) ) ]
