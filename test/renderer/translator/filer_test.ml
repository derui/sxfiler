open Mocha_of_ocaml
module Tr = Sxfiler_renderer_translator
module T = Sxfiler_rpc.Types

let suite () =
  "Node translator" >::: [
    "should be able to convert between JavaScript and OCaml" >:: (fun () ->
        let stat = T.File_stat.{
            mode = Int32.to_string @@ Int32.of_int 0o775;
            uid = 1000;
            gid = 1000;
            atime = "100";
            mtime = "1000";
            ctime = "10000";
            size = Int64.to_string Int64.max_int;
            is_directory = true;
            is_file = false;
            is_symlink = true;
          }
        in
        let node = T.Node.{
            name = "bar";
            parent_directory = "/foo";
            stat;
            link_path = None
          }
        in
        let data = T.Filer.{
            id = "id";
            location = "bar";
            nodes = [node];
            history = {T.Location_history.records = []; max_records = 100}
          }
        in
        assert_ok (data = Tr.Filer.(of_js @@ to_js data))
      );
  ]
