open Sxfiler_core
open Mocha_of_ocaml
module D = Sxfiler_domain
module T = Sxfiler_renderer_translator

let suite () =
  "Location history translator" >::: [
    "should be able to convert between JavaScript and OCaml" >:: (fun () ->
        let data = D.Location_history.make () in
        let record = D.Location_record.record_of
            ~location:(Path.of_string @@ Filename.concat "foo" "bar")
            (module struct
              let unixtime () = 0L
            end) in
        let data = D.Location_history.add_record data ~record in
        assert_ok (data = T.Location_history.(of_js @@ to_js data))
      );
  ]
