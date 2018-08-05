open Mocha_of_ocaml
module D = Sxfiler_completion.Domain
module T = Sxfiler_renderer_translator

let suite () =
  "Completion translator" >::: [
    "should be able to convert between JavaScript and OCaml" >:: (fun () ->
        let data = {D.Item.id = "value";
                    value = "foobar"
                   }
        in
        assert_ok (data = T.Completion.Item.(of_js @@ to_js data))
      );
    "should be able to convert candidate between JavaScript and OCaml" >::  (fun () ->
        let data = {D.Candidate.start = 1;
                    length = 20;
                    value = {D.Item.id = "foo"; value = "value"};
                   }
        in
        assert_ok (data = T.Completion.Candidate.(of_js @@ to_js data))
      );
  ]
