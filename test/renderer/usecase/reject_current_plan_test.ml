open Mocha_of_ocaml
open Mocha_of_ocaml_async
module C = Sxfiler_renderer_core
module S = Sxfiler_renderer_service
module U = Sxfiler_renderer_usecase

let () =
  "Reject current plan use case"
  >::: [ ( "should send message to reject the plan"
           >:- fun () ->
             let module S : S.Plan.S = struct
               include Util.Service_stub.Plan

               let reject _ = Lwt.return_unit
             end in
             let module Target = U.Reject_current_plan.Make (S) in
             let module T = Sxfiler_rpc.Types in
             let instance = Target.create T.Plan.empty in
             let module D = struct
               let message_ = ref None
               let dispatch m = message_ := Some m
             end in
             let%lwt () = Target.execute instance (module Util.Dummy_dispatcher (D)) in
             Lwt.return @@ assert_ok (!D.message_ = Some C.Message.(Command Reject)) ) ]
