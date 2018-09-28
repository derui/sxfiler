open Sxfiler_core
module D = Sxfiler_domain
module U = Sxfiler_usecase

let test_set =
  [ Alcotest_lwt.test_case "call service via notification" `Quick (fun _ () ->
        let s, f = Spy.wrap (Fun.const Lwt.return_unit) in
        let module Service = struct
          let send = f
        end in
        let module F = struct
          let id = Uuidm.v4_gen (Random.get_state ()) ()
          let create body = D.Notification.make ~id ~body
        end in
        let module Usecase = U.Notification.Notify.Make (F) (Service) in
        let notification = D.Notification.(OneShot {message = "foo"; level = Info}) in
        let%lwt ret = Usecase.execute {notification} in
        Alcotest.(check @@ result unit unit) "result" (Ok ()) ret ;
        Alcotest.(check int) "count" 1 Spy.Wrap.(called_count s) ;
        Alcotest.(check @@ list @@ of_pp Fmt.nop)
          "sent"
          Spy.Wrap.(called_args s)
          [F.create notification] ;
        Lwt.return_unit ) ]
