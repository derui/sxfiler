open Mocha_of_ocaml
module Tr = Sxfiler_renderer_translator
module T = Sxfiler_rpc.Types

let suite () =
  "Location record translator" >::: [
    "should be able to convert between JavaScript and OCaml" >:: (fun () ->
        let data = T.Location_record.{
            location = "foo/bar";
            timestamp = "100";
          }
        in
        assert_ok (data = Tr.Location_record.(of_js @@ to_js data))
      );
  ]
