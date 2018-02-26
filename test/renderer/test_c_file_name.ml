open Mocha_of_ocaml
open Snap_shot_it_of_ocaml
module R = Reactjscaml
module S = Sxfiler_renderer

let suite () =
  "File name component" >::: [
    "should who filename normally" >:: (fun () ->
        let module C = S.C_file_name in
        let e = R.element ~props:(object%js
            val fileName = Js.string "sample.txt"
            val isDirectory = Js.bool false
            val isSymbolicLink = Js.bool false
          end) C.component
        in
        let renderer = new%js R.Test_renderer.shallow_ctor in
        renderer##render e;
        let output = renderer##getRenderOutput in
        snapshot output;
        assert_ok true
      );
    "should make another color if item is directory" >:: (fun () ->
        let module C = S.C_file_name in
        let e = R.element ~props:(object%js
            val fileName = Js.string "sample"
            val isDirectory = Js.bool true
            val isSymbolicLink = Js.bool false
          end) C.component
        in
        let renderer = new%js R.Test_renderer.shallow_ctor in
        renderer##render e;
        let output = renderer##getRenderOutput in
        snapshot output;
        assert_ok true
      );
    "should make another color if item is symbolic link" >:: (fun () ->
        let module C = S.C_file_name in
        let e = R.element ~props:(object%js
            val fileName = Js.string "sample.txt"
            val isDirectory = Js.bool false
            val isSymbolicLink = Js.bool true
          end) C.component
        in
        let renderer = new%js R.Test_renderer.shallow_ctor in
        renderer##render e;
        let output = renderer##getRenderOutput in
        snapshot output;
        assert_ok true
      )

  ]