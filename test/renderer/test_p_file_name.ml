open Mocha_of_ocaml
open Snap_shot_it_of_ocaml
module R = Jsoo_reactjs
module S = Sxfiler_renderer

let suite () =
  "File name component" >::: [
    "should who filename normally" >:: (fun () ->
        let module C = S.P_file_name in
        let e = R.create_element ~props:(object%js
            val name = "/foo/bar/sample.txt"
            val isDirectory = false
            val isSymbolicLink = false
          end) C.t
        in
        let renderer = new%js R.Test_renderer.shallow_ctor in
        renderer##render e;
        let output = renderer##getRenderOutput in
        snapshot output;
        assert_ok true
      );
    "should make another color if item is directory" >:: (fun () ->
        let module C = S.P_file_name in
        let e = R.create_element ~props:(object%js
            val name =  "/foo/bar/"
            val isDirectory = true
            val isSymbolicLink = false
          end) C.t
        in
        let renderer = new%js R.Test_renderer.shallow_ctor in
        renderer##render e;
        let output = renderer##getRenderOutput in
        snapshot output;
        assert_ok true
      );
    "should make another color if item is symbolic link" >:: (fun () ->
        let module C = S.P_file_name in
        let e = R.create_element ~props:(object%js
            val name = "/foo/bar/sample.txt"
            val isDirectory = false
            val isSymbolicLink = true
          end) C.t
        in
        let renderer = new%js R.Test_renderer.shallow_ctor in
        renderer##render e;
        let output = renderer##getRenderOutput in
        snapshot output;
        assert_ok true
      );
  ]
