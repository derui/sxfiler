open Mocha_of_ocaml
module Tr = Sxfiler_renderer_translator
module T = Sxfiler_rpc.Types

let suite () =
  "Condition translator" >::: [
    "should be able to convert between JavaScript and OCaml" >:: (fun () ->
        let data = T.Condition.{
            enabled_contexts = ["foo"]
          } in
        assert_ok (data = Tr.Condition.(of_js @@ to_js data))
      );
  ]
