module D = Sxfiler_domain
module Tr = Sxfiler_server_translator.Notification
module T = Sxfiler_rpc.Types

let test_set =
  [ ( "can translate to/from domain"
    , `Quick
    , fun () ->
      let id = Uuidm.v4_gen (Random.get_state ()) () in
      let data =
        D.Notification.make ~id ~level:D.Notification.Level.Info
          ~body:(D.Notification.OneShot {message = "foo"})
      in
      let expected =
        { T.Notification.id = Uuidm.to_string id
        ; level = D.Notification.Level.Info
        ; body = D.Notification.OneShot {message = "foo"} }
      in
      Alcotest.(check @@ of_pp Fmt.nop) "domain" expected (Tr.of_domain data) )
  ; ( "can translate to/from yojson"
    , `Quick
    , fun () ->
      let id = Uuidm.v4_gen (Random.get_state ()) () in
      let data =
        D.Notification.make ~id ~level:D.Notification.Level.Info
          ~body:(D.Notification.Progress {process = "foo"; targeted = 10.0; current = 1.})
      in
      Alcotest.(check string)
        "yojson"
        {|{"type":"progress","body":{"targeted":10.0,"current":1.0,"process":"foo"},"id":"c0615a9d-3844-4b56-91cc-4b1968ddd7ab","level":0}|}
        (Tr.of_domain data |> Tr.to_yojson |> Yojson.Safe.to_string) ) ]
