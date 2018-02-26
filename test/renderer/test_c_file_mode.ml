open Mocha_of_ocaml
open Snap_shot_it_of_ocaml
module R = Reactjscaml
module S = Sxfiler_renderer

let suite () =
  "File mode component" >::: [
    "should show current permission of file" >:: (fun () ->
        let module F = S.C_file_mode in
        let e = R.element ~props:(object%js
            val mode = Js.number_of_float @@ float_of_int 0o644
          end) F.component
        in
        let renderer = new%js R.Test_renderer.shallow_ctor in
        renderer##render e;
        let output = renderer##getRenderOutput in
        snapshot output;
        assert_ok true
      );
    "should be hyphen if no any permission " >:: (fun () ->
        let module F = S.C_file_mode in
        let e = R.element ~props:(object%js
            val mode = Js.number_of_float @@ float_of_int 0
          end) F.component
        in
        let renderer = new%js R.Test_renderer.shallow_ctor in
        renderer##render e;
        let output = renderer##getRenderOutput in
        snapshot output;
        assert_ok true
      );
    "should be able to show symlink bit if mode contains symlink bit" >:: (fun () ->
        let module F = S.C_file_mode in
        let e = R.element ~props:(object%js
            val mode = Js.number_of_float @@ float_of_int @@ 0o120000 lor 0o777
          end) F.component
        in
        let renderer = new%js R.Test_renderer.shallow_ctor in
        renderer##render e;
        let output = renderer##getRenderOutput in
        snapshot output;
        assert_ok true
      );
    "should be able to show directory bit if mode contains directory bit" >:: (fun () ->
        let module F = S.C_file_mode in
        let e = R.element ~props:(object%js
            val mode = Js.number_of_float @@ float_of_int @@ 0o040000 lor 0o755
          end) F.component
        in
        let renderer = new%js R.Test_renderer.shallow_ctor in
        renderer##render e;
        let output = renderer##getRenderOutput in
        snapshot output;
        assert_ok true
      );
  ]
