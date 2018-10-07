open Mocha_of_ocaml
open Mocha_of_ocaml_async
module C = Sxfiler_renderer_core
module S = Sxfiler_renderer_service
module U = Sxfiler_renderer_usecase
module T = Sxfiler_rpc.Types
module D = Sxfiler_domain

let () =
  "Notify a notification use case"
  >::: [ ( "should send notification and timeout for message"
           >:- fun () ->
             let module S = struct
               let arg = ref 0.0

               let sleep v =
                 arg := v ;
                 Lwt.return_unit
             end in
             let module Target = U.Notify_message.Make (S) in
             let data =
               { T.Notification.id = "foo"
               ; level = D.Notification.Level.Info
               ; body = OneShot {message = "foo"} }
             in
             let instance = Target.create data in
             let module D = struct
               let messages_ = ref []
               let dispatch m = messages_ := m :: !messages_
             end in
             let%lwt () = Target.execute instance (module Util.Dummy_dispatcher (D)) in
             Infix.(
               assert_ok
                 ( List.rev !D.messages_
                   = [C.Message.(Notify data); C.Message.Timeout_notification "foo"] )
               <|> assert_eq U.Notify_message.timeout !S.arg)
             |> Lwt.return )
       ; ( "should send message without timeout if progress"
           >:- fun () ->
             let module S = struct
               let arg = ref None

               let sleep v =
                 arg := Some v ;
                 Lwt.return_unit
             end in
             let module Target = U.Notify_message.Make (S) in
             let data =
               { T.Notification.id = "foo"
               ; level = D.Notification.Level.Info
               ; body = Progress {targeted = 1.0; current = 0.5; process = "process"} }
             in
             let instance = Target.create data in
             let module D = struct
               let messages_ = ref []
               let dispatch m = messages_ := m :: !messages_
             end in
             let%lwt () = Target.execute instance (module Util.Dummy_dispatcher (D)) in
             Infix.(
               assert_ok (List.rev !D.messages_ = [C.Message.(Notify data)])
               <|> assert_ok (!S.arg = None))
             |> Lwt.return ) ]
