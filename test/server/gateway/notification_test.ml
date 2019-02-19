module D = Sxfiler_domain
module S = Sxfiler_server
module U = Sxfiler_usecase
module C = Sxfiler_server_core
module G = Sxfiler_server_gateway
module Tr = Sxfiler_server_translator

let test_set =
  [ Alcotest_lwt.test_case "notify a message" `Quick (fun _ () ->
        let spy, f = Spy.wrap (fun (_ : U.Notification.Notify.Type.input) -> Lwt.return_ok ()) in
        let module Usecase : U.Notification.Notify.S = struct
          include U.Notification.Notify.Type

          let execute = f
        end in
        let module Gateway = G.Notification.Notify_message.Make (Usecase) in
        let%lwt _ = Gateway.handle {message = "message"; level = Tr.Notification.Level.Info} in
        Alcotest.(check @@ list @@ of_pp Fmt.nop)
          "message"
          [ { U.Notification.Notify.Type.notification = D.Notification.Message "message"
            ; level = Info } ]
          (Spy.Wrap.called_args spy) ;
        Lwt.return_unit ) ]
