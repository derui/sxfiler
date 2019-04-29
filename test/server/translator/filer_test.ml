open Sxfiler_core
module D = Sxfiler_domain
module Tr = Sxfiler_server_translator

let data =
  D.Filer.Factory.create ~name:"id"
    ~file_tree:Test_fixtures.(File_tree.empty_tree (Path.of_string "/bar"))
    ~sort_order:D.Types.Sort_type.Date ()

let test_set =
  [ ( "can translate to/from domain"
    , `Quick
    , fun () ->
        Alcotest.(check @@ of_pp D.Filer.pp) "domain" data Tr.Filer.(to_domain @@ of_domain data)
    )
  ; ( "can translate to/from yojson"
    , `Quick
    , fun () ->
        let data = Tr.Filer.of_domain data in
        Alcotest.(check @@ result (of_pp Tr.Filer.pp) (of_pp Fmt.nop))
          "yojson" (Ok data)
          (Tr.Filer.of_yojson @@ Tr.Filer.to_yojson data) ) ]
