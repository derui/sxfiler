open Mocha_of_ocaml
open Snap_shot_it_of_ocaml
module R = Jsoo_reactjs
module S = Sxfiler_renderer

let () =
  "File size component"
  >::: [ ( "should show size of the file"
           >:: fun () ->
             let module F = S.P_file_size in
             let e =
               R.create_element
                 ~props:
                   (object%js
                     val size = 0L
                   end)
                 F.t
             in
             let renderer = new%js R.Test_renderer.shallow_ctor in
             renderer##render e ;
             let output = renderer##getRenderOutput in
             snapshot output ; assert_ok true )
       ; ( "should show kilobyte if the size more than 1024 byte"
           >:: fun () ->
             let module F = S.P_file_size in
             let e =
               R.create_element
                 ~props:
                   (object%js
                     val size = 1024L
                   end)
                 F.t
             in
             let renderer = new%js R.Test_renderer.shallow_ctor in
             renderer##render e ;
             let output = renderer##getRenderOutput in
             snapshot output ; assert_ok true )
       ; ( "should show megabyte if the size more than 1024 KBytes"
           >:: fun () ->
             let module F = S.P_file_size in
             let e =
               R.create_element
                 ~props:
                   (object%js
                     val size = Int64.mul 1024L 1024L
                   end)
                 F.t
             in
             let renderer = new%js R.Test_renderer.shallow_ctor in
             renderer##render e ;
             let output = renderer##getRenderOutput in
             snapshot output ; assert_ok true )
       ; ( "should show Gigabytes if the size more than 1024 MBytes"
           >:: fun () ->
             let module F = S.P_file_size in
             let e =
               R.create_element
                 ~props:
                   (object%js
                     val size = Int64.(mul 1024L @@ mul 1024L 1024L)
                   end)
                 F.t
             in
             let renderer = new%js R.Test_renderer.shallow_ctor in
             renderer##render e ;
             let output = renderer##getRenderOutput in
             snapshot output ; assert_ok true ) ]
