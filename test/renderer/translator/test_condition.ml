open Mocha_of_ocaml
module D = Sxfiler_domain
module T = Sxfiler_renderer_translator

let suite () =
  "Condition translator" >::: [
    "should be able to convert between JavaScript and OCaml" >:: (fun () ->
        let data = D.Condition.of_list [D.Condition.On_completing] in
        assert_ok (data = T.Condition.(of_js @@ to_js data))
      );
  ]
