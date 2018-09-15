open Mocha_of_ocaml
open Snap_shot_it_of_ocaml
module D = Sxfiler_domain
module R = Jsoo_reactjs
module S = Sxfiler_renderer

let () =
  "Node plan operation"
  >::: [ ( "should be able to show operation to append"
           >:: fun () ->
             let module C = S.P_node_plan_operation in
             let e =
               R.create_element
                 ~props:
                   (object%js
                     val operation = D.Plan.Operation.Append
                   end)
                 C.t
             in
             let renderer = new%js R.Test_renderer.shallow_ctor in
             renderer##render e ;
             let output = renderer##getRenderOutput in
             snapshot output ; assert_ok true )
       ; ( "should be able to show operation to delete"
           >:: fun () ->
             let module C = S.P_node_plan_operation in
             let e =
               R.create_element
                 ~props:
                   (object%js
                     val operation = D.Plan.Operation.Delete
                   end)
                 C.t
             in
             let renderer = new%js R.Test_renderer.shallow_ctor in
             renderer##render e ;
             let output = renderer##getRenderOutput in
             snapshot output ; assert_ok true )
       ; ( "should be able to show operation for remained"
           >:: fun () ->
             let module C = S.P_node_plan_operation in
             let e =
               R.create_element
                 ~props:
                   (object%js
                     val operation = D.Plan.Operation.Remained
                   end)
                 C.t
             in
             let renderer = new%js R.Test_renderer.shallow_ctor in
             renderer##render e ;
             let output = renderer##getRenderOutput in
             snapshot output ; assert_ok true )
       ; ( "should be able to show operation to conflict"
           >:: fun () ->
             let module C = S.P_node_plan_operation in
             let e =
               R.create_element
                 ~props:
                   (object%js
                     val operation = D.Plan.Operation.Conflict
                   end)
                 C.t
             in
             let renderer = new%js R.Test_renderer.shallow_ctor in
             renderer##render e ;
             let output = renderer##getRenderOutput in
             snapshot output ; assert_ok true ) ]
