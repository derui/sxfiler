open Mocha_of_ocaml
open Mocha_of_ocaml_async
module C = Sxfiler_renderer_core
module S = Sxfiler_renderer_service
module U = Sxfiler_renderer_usecase

let () =
  "Approving current plan use case"
  >::: [ ( "should send message to approve the plan"
           >:- fun () ->
             let module I = (val C.Usecase.make_instance (module U.Approve_current_plan) ~param:()) in
             let module D = struct
               let message_ = ref None
               let dispatch m = message_ := Some m
             end in
             let%lwt () = I.(Usecase.execute this (module Util.Dummy_dispatcher (D))) in
             Lwt.return @@ assert_ok (!D.message_ = Some C.Message.(Command Approve)) ) ]
