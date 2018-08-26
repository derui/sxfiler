open Mocha_of_ocaml
module Tr = Sxfiler_renderer_translator
module T = Sxfiler_rpc.Types

let () =
  "Node translator" >::: [
    "should be able to convert between JavaScript and OCaml" >:: (fun () ->
        let stat = T.File_stat.{
            mode = Int32.(to_string @@ of_int 0o775);
            uid = 1000;
            gid = 1000;
            atime = Int64.to_string @@ (Int64.of_int 100);
            mtime = Int64.to_string @@ (Int64.of_int 1000);
            ctime = Int64.to_string @@ (Int64.of_int 10000);
            size = Int64.to_string @@ (Int64.max_int);
            is_directory = true;
            is_file = false;
            is_symlink = true;
          } in

        let data = T.Node.{
            name = "bar";
            parent_directory = "foo";
            stat;
            link_path = None;
          }
        in
        assert_ok (data = Tr.Node.(of_js @@ to_js data))
      );
  ]
