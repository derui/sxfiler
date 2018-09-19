open Sxfiler_core
module D = Sxfiler_domain
module Tr = Sxfiler_server_translator
module T = Sxfiler_rpc.Types

let data =
  { D.Node.id = "id"
  ; full_path = Path.of_string "/bar"
  ; stat =
      D.File_stat.make ~mode:(Int32.of_int 0o775) ~uid:1000 ~gid:1000 ~atime:(Int64.of_int 100)
        ~mtime:(Int64.of_int 1000) ~ctime:(Int64.of_int 10000) ~size:Int64.max_int
        ~is_directory:true ~is_file:false ~is_symlink:true
  ; link_path = Some "/foo" }

let test_set =
  [ ( "can translate to/from domain"
    , `Quick
    , fun () ->
      let expected =
        { T.Node.id = "id"
        ; name = "bar"
        ; stat = Tr.File_stat.of_domain data.stat
        ; parent_directory = "/"
        ; link_path = Some "/foo" }
      in
      Alcotest.(check @@ of_pp Fmt.nop) "domain" expected (Tr.Node.of_domain data) )
  ; ( "can translate to/from yojson"
    , `Quick
    , fun () ->
      let data = Tr.Node.of_domain data in
      Alcotest.(check @@ result (of_pp Fmt.nop) (of_pp Fmt.nop))
        "yojson" (Ok data)
        (Tr.Node.of_yojson @@ Tr.Node.to_yojson data) ) ]
