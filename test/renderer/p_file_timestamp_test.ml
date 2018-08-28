open Mocha_of_ocaml
open Snap_shot_it_of_ocaml
module R = Jsoo_reactjs
module S = Sxfiler_renderer

let () =
  "File time component"
  >::: [ ( "should show time string"
           >:: fun () ->
             let module C = S.P_file_timestamp in
             let e =
               R.create_element
                 ~props:
                   (object%js
                     val timestamp = 100.0
                   end)
                 C.t
             in
             let renderer = new%js R.Test_renderer.shallow_ctor in
             renderer##render e ;
             let output = renderer##getRenderOutput in
             snapshot output ; assert_ok true ) ]
