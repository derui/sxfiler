open Mocha_of_ocaml
open Mocha_of_ocaml_async
module C = Sxfiler_renderer_core
module T = Sxfiler_rpc.Types
module S = Sxfiler_renderer_service
module U = Sxfiler_renderer_usecase

let () =
  "Move nodes between filers use case"
  >::: [ ( "should call move nodes and update filers"
           >:- fun () ->
             let expected =
               { T.Filer.id = "foo"
               ; location = "loc"
               ; nodes = []
               ; history = T.Location_history.{records = []; max_records = 100} }
             in
             let get_args = ref [] in
             let module S : S.Filer.S = struct
               include Util.Service_stub.Filer

               let get arg =
                 get_args := arg :: !get_args ;
                 Lwt.return_ok expected

               let move_nodes _ = Lwt.return_unit
             end in
             let module Target = U.Filer_move_nodes.Make (S) in
             let module I =
               ( val C.Usecase.make_instance
                   (module Target)
                   ~param:{workbench_id = "foo"; source = []; dest = []} )
             in
             let module D = struct
               let message_ = ref []
               let dispatch m = message_ := m :: !message_
             end in
             let%lwt () = I.(Usecase.execute this (module Util.Dummy_dispatcher (D))) in
             let open Infix in
             let test =
               assert_ok (List.length !get_args = 2) <|> assert_ok (List.length !D.message_ = 3)
             in
             Lwt.return test ) ]
