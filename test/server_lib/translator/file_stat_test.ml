module D = Sxfiler_domain
module T = Sxfiler_server_translator.File_stat

let data =
  D.File_stat.make ~mode:(Int32.of_int 0o775) ~uid:1000 ~gid:1000 ~atime:(Int64.of_int 100)
    ~mtime:(Int64.of_int 1000) ~ctime:(Int64.of_int 10000) ~size:Int64.max_int ~is_directory:true
    ~is_file:false ~is_symlink:true

let test_set =
  [ ( "can translate to/from domain"
    , `Quick
    , fun () -> Alcotest.(check @@ of_pp Fmt.nop) "domain" data (T.to_domain @@ T.of_domain data)
    )
  ; ( "can translate to/from yojson"
    , `Quick
    , fun () ->
      let data = T.of_domain data in
      Alcotest.(check @@ result (of_pp Fmt.nop) (of_pp Fmt.nop))
        "yojson" (Ok data)
        (T.of_yojson @@ T.to_yojson data) ) ]
