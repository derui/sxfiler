open Mocha_of_ocaml
module Tr = Sxfiler_renderer_translator
module T = Sxfiler_rpc.Types

let () =
  "File stat translator"
  >::: [ ( "should be able to convert between JavaScript and OCaml"
           >:: fun () ->
             let data =
               { T.Key_map.bindings =
                   [ {T.Key_map.key = "j"; action = "value"; condition = T.Condition.empty}
                   ; {T.Key_map.key = "k"; action = "value2"; condition = T.Condition.empty} ] }
             in
             assert_ok (data = Tr.Key_map.(of_js @@ to_js data)) ) ]
