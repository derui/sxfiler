open Sxfiler_core
module D = Sxfiler_domain
module Tr = Sxfiler_server_translator

let data = Test_fixtures.(File_list.empty_list (Path.of_string "/bar"))

let test_set =
  [
    ( "can translate to/from domain",
      `Quick,
      fun () ->
        Alcotest.(check @@ of_pp D.File_list.pp)
          "domain" data
          Tr.File_list.(to_domain @@ of_domain data) );
  ]
