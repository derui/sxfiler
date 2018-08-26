open Mocha_of_ocaml
module Tr = Sxfiler_renderer_translator
module T = Sxfiler_rpc.Types

let () =
  "Completion translator" >::: [
    "should be able to convert between JavaScript and OCaml" >:: (fun () ->
        let data = {T.Completion.Item.id = "value";
                    value = "foobar"
                   }
        in
        assert_ok (data = Tr.Completion.Item.(of_js @@ to_js data))
      );
    "should be able to convert candidate between JavaScript and OCaml" >::  (fun () ->
        let data = {T.Completion.Candidate.start = 1;
                    length = 20;
                    value = {T.Completion.Item.id = "foo"; value = "value"};
                   }
        in
        assert_ok (data = Tr.Completion.Candidate.(of_js @@ to_js data))
      );
  ]
