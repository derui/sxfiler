open Mocha_of_ocaml
open Mocha_of_ocaml_async
module C = Sxfiler_renderer_core
module S = Sxfiler_renderer_service
module U = Sxfiler_renderer_usecase

let () =
  "Swap filer use case"
  >::: [ ( "should send message to swap filer position"
           >:- fun () ->
             let module Target = U.Swap_filer in
             let instance = Target.create () in
             let module D = struct
               let message_ = ref None
               let dispatch m = message_ := Some m
             end in
             let%lwt () = Target.execute instance (module Util.Dummy_dispatcher (D)) in
             Lwt.return @@ assert_ok (!D.message_ = Some C.Message.Swap_filer) ) ]
