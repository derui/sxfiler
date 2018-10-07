open Mocha_of_ocaml
module Tr = Sxfiler_renderer_translator
module T = Sxfiler_rpc.Types

let () =
  "Notification translator"
  >::: [ ( "should be able to convert between JavaScript and OCaml"
           >:: fun () ->
             let module D = Sxfiler_domain in
             let data =
               Js.Unsafe.coerce
                 (object%js
                   val id = Js.string "sample"

                   val level = D.Notification.Level.to_int Info

                   val _type = Js.string "message"

                   val body_message = Js.string "a message"
                 end)
             in
             let expected =
               { T.Notification.id = "sample"
               ; level = D.Notification.Level.Info
               ; body = OneShot {message = "a message"} }
             in
             assert_ok (expected = Tr.Notification.(of_js data)) )
       ; ( "should be able to convert between JavaScript and OCaml for progress"
           >:: fun () ->
             let module D = Sxfiler_domain in
             let data =
               Js.Unsafe.coerce
                 (object%js
                   val id = Js.string "sample"

                   val level = D.Notification.Level.to_int Info

                   val _type = Js.string "progress"

                   val body_progress =
                     object%js
                       val targeted = 1.

                       val current = 0.5

                       val process = Js.string "process"
                     end
                 end)
             in
             let expected =
               { T.Notification.id = "sample"
               ; level = D.Notification.Level.Info
               ; body = Progress {targeted = 1.; current = 0.5; process = "process"} }
             in
             assert_ok (expected = Tr.Notification.(of_js data)) ) ]
