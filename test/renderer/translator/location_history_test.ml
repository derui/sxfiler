open Mocha_of_ocaml
module Tr = Sxfiler_renderer_translator
module T = Sxfiler_rpc.Types

let () =
  "Location history translator"
  >::: [ ( "should be able to convert between JavaScript and OCaml"
           >:: fun () ->
             let record = {T.Location_record.location = "foo/bar"; timestamp = "10000"} in
             let data = T.Location_history.{records = [record]; max_records = 100} in
             assert_ok (data = Tr.Location_history.(of_js @@ to_js data)) ) ]
