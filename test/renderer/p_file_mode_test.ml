open Mocha_of_ocaml
open Snap_shot_it_of_ocaml
module R = Jsoo_reactjs
module S = Sxfiler_renderer

let () =
  "File mode component"
  >::: [ ( "should show current permission of file"
           >:: fun () ->
             let module F = S.P_file_mode in
             let e =
               R.create_element
                 ~props:
                   (object%js
                     val mode = 0o644l
                   end)
                 F.t
             in
             let renderer = new%js R.Test_renderer.shallow_ctor in
             renderer##render e ;
             let output = renderer##getRenderOutput in
             snapshot output ; assert_ok true )
       ; ( "should be hyphen if no any permission "
           >:: fun () ->
             let module F = S.P_file_mode in
             let e =
               R.create_element
                 ~props:
                   (object%js
                     val mode = 0l
                   end)
                 F.t
             in
             let renderer = new%js R.Test_renderer.shallow_ctor in
             renderer##render e ;
             let output = renderer##getRenderOutput in
             snapshot output ; assert_ok true )
       ; ( "should be able to show symlink bit if mode contains symlink bit"
           >:: fun () ->
             let module F = S.P_file_mode in
             let e =
               R.create_element
                 ~props:
                   (object%js
                     val mode = Int32.logor 0o120000l 0o777l
                   end)
                 F.t
             in
             let renderer = new%js R.Test_renderer.shallow_ctor in
             renderer##render e ;
             let output = renderer##getRenderOutput in
             snapshot output ; assert_ok true )
       ; ( "should be able to show directory bit if mode contains directory bit"
           >:: fun () ->
             let module F = S.P_file_mode in
             let e =
               R.create_element
                 ~props:
                   (object%js
                     val mode = Int32.logor 0o040000l 0o755l
                   end)
                 F.t
             in
             let renderer = new%js R.Test_renderer.shallow_ctor in
             renderer##render e ;
             let output = renderer##getRenderOutput in
             snapshot output ; assert_ok true ) ]
