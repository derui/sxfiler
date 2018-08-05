open Mocha_of_ocaml
module D = Sxfiler_domain
module T = Sxfiler_renderer_translator

let suite () =
  "File stat translator" >::: [
    "should be able to convert between JavaScript and OCaml" >:: (fun () ->
        let data = List.fold_left (fun keymap (key, value) ->
            D.Key_map.add keymap ~condition:D.Condition.empty
              ~key ~value
          )
            (D.Key_map.make ())
            [Sxfiler_kbd.make "j", "value";
             Sxfiler_kbd.make "k", "value2";
            ] in
        assert_ok (data = T.Key_map.(of_js @@ to_js data))
      );
  ]
