open Sxfiler_core
open Mocha_of_ocaml
module D = Sxfiler_domain
module T = Sxfiler_renderer_translator

let suite () =
  "Location record translator" >::: [
    "should be able to convert between JavaScript and OCaml" >:: (fun () ->
        let data = D.Location_record.record_of
            ~location:(Path.of_string @@ Filename.concat "foo" "bar")
            (module struct
              let unixtime () = 0L
            end) in
        assert_ok (data = T.Location_record.(of_js @@ to_js data))
      );
  ]
