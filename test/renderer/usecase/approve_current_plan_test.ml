open Mocha_of_ocaml
open Mocha_of_ocaml_async
module C = Sxfiler_renderer_core
module S = Sxfiler_renderer_service
module U = Sxfiler_renderer_usecase
module T = Sxfiler_rpc.Types

let () =
  "Approving current plan use case"
  >::: [ ( "should send message to approve the plan"
           >:- fun () ->
             let param =
               { T.Plan.workbench_id = "id"
               ; source = [{T.Plan.operation = Delete; node = T.Node.empty}]
               ; dest = [{T.Plan.operation = Append; node = T.Node.empty}] }
             in
             let module I = (val C.Usecase.make_instance (module U.Approve_current_plan) ~param) in
             let module D = struct
               let message_ = ref None
               let dispatch m = message_ := Some m
             end in
             let%lwt () = I.(Usecase.execute this (module Util.Dummy_dispatcher (D))) in
             Lwt.return @@ assert_ok (!D.message_ = Some C.Message.(Command Approve)) )
       ; ( "should send message to force to handle conflict in the plan"
           >:- fun () ->
             let param =
               { T.Plan.workbench_id = "id"
               ; source = [{T.Plan.operation = Conflict; node = T.Node.empty}]
               ; dest = [{T.Plan.operation = Conflict; node = T.Node.empty}] }
             in
             let module I = (val C.Usecase.make_instance (module U.Approve_current_plan) ~param) in
             let module D = struct
               let message_ = ref None
               let dispatch m = message_ := Some m
             end in
             let%lwt () = I.(Usecase.execute this (module Util.Dummy_dispatcher (D))) in
             Lwt.return @@ assert_ok (!D.message_ = Some C.Message.(Command Remains_conflict)) ) ]
