open Sxfiler_core
open Mocha_of_ocaml
module D = Sxfiler_domain
module T = Sxfiler_renderer_translator

let suite () =
  "Node translator" >::: [
    "should be able to convert between JavaScript and OCaml" >:: (fun () ->
        let stat = D.File_stat.make
            ~mode:(Int32.of_int 0o775)
            ~uid:1000
            ~gid:1000
            ~atime:(Int64.of_int 100)
            ~mtime:(Int64.of_int 1000)
            ~ctime:(Int64.of_int 10000)
            ~size:(Int64.max_int)
            ~is_directory:true
            ~is_file:false
            ~is_symlink:true
        in
        let data = D.Node.make
            ~full_path:(Path.of_string @@ Filename.concat "foo" "bar")
            ~stat ~parent_directory:"foo" ~link_path:None
        in
        assert_ok (data = T.Node.(of_js @@ to_js data))
      );
  ]
