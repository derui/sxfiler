open Mocha_of_ocaml
open Snap_shot_it_of_ocaml
module R = Jsoo_reactjs
module S = Sxfiler_renderer

let suite () =
  "File size component" >::: [
    "should show size of the file" >:: (fun () ->
        let module F = S.C_file_size in
        let e = R.create_element ~props:(object%js
            val size = Js.number_of_float 0.0
          end) F.component
        in
        let renderer = new%js R.Test_renderer.shallow_ctor in
        renderer##render e;
        let output = renderer##getRenderOutput in
        snapshot output;
        assert_ok true
      );
    "should show kilobyte if the size more than 1024 byte" >:: (fun () ->
        let module F = S.C_file_size in
        let e = R.create_element ~props:(object%js
            val size = Js.number_of_float 1024.0
          end) F.component
        in
        let renderer = new%js R.Test_renderer.shallow_ctor in
        renderer##render e;
        let output = renderer##getRenderOutput in
        snapshot output;
        assert_ok true
      );
    "should show megabyte if the size more than 1024 KBytes" >:: (fun () ->
        let module F = S.C_file_size in
        let e = R.create_element ~props:(object%js
            val size = Js.number_of_float @@ 1024.0 *. 1024.0
          end) F.component
        in
        let renderer = new%js R.Test_renderer.shallow_ctor in
        renderer##render e;
        let output = renderer##getRenderOutput in
        snapshot output;
        assert_ok true
      );
    "should show Gigabytes if the size more than 1024 MBytes" >:: (fun () ->
        let module F = S.C_file_size in
        let e = R.create_element ~props:(object%js
            val size = Js.number_of_float @@ 1024.0 *. 1024.0 *. 1024.0
          end) F.component
        in
        let renderer = new%js R.Test_renderer.shallow_ctor in
        renderer##render e;
        let output = renderer##getRenderOutput in
        snapshot output;
        assert_ok true
      );
    "should ignore fraction value less than 0.1" >:: (fun () ->
        let module F = S.C_file_size in
        let e = R.create_element ~props:(object%js
            val size = Js.number_of_float @@ 1024.0 *. 3.51
          end) F.component
        in
        let renderer = new%js R.Test_renderer.shallow_ctor in
        renderer##render e;
        let output = renderer##getRenderOutput in
        snapshot output;
        assert_ok true
      );
  ]
