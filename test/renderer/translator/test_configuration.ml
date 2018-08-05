open Mocha_of_ocaml
module D = Sxfiler_domain
module T = Sxfiler_renderer_translator

let suite () =
  "Configuration translator" >::: [
    "should be able to convert between JavaScript and OCaml" >:: (fun () ->
        let data = D.Configuration.default in
        assert_ok (data = T.Configuration.(of_js @@ to_js data))
      );
  ]
