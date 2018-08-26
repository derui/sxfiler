open Mocha_of_ocaml
module Tr = Sxfiler_renderer_translator
module T = Sxfiler_rpc.Types

let () =
  "Configuration translator" >::: [
    "should be able to convert between JavaScript and OCaml" >:: (fun () ->
        let data = T.Configuration.{
            default_sort_order = 1;
          } in
        assert_ok (data = Tr.Configuration.(of_js @@ to_js data))
      );
  ]