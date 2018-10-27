open Mocha_of_ocaml
open Mocha_of_ocaml_async
module C = Sxfiler_renderer_core
module S = Sxfiler_renderer_service
module U = Sxfiler_renderer_usecase

let () =
  "Toggle mark current node use case"
  >::: [ ( "should send two messages"
           >:- fun () ->
             let module Target = U.Filer_toggle_mark in
             let module T = Sxfiler_rpc.Types in
             let instance = Target.create () in
             let module D = struct
               let message_ = ref []
               let dispatch m = message_ := m :: !message_
             end in
             let%lwt () = Target.execute instance (module Util.Dummy_dispatcher (D)) in
             Lwt.return @@ assert_ok (!D.message_ = C.Message.[Move_cursor_to_next; Toggle_mark]) ) ]
