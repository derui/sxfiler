module C = Sxfiler_domain.Completer
module CM = Sxfiler_infrastructure.Forward_match_completer
module F = Test_fixtures

let test_set =
  [
    Alcotest_lwt.test_case_sync "can return candidates from collection by forward match" `Quick (fun () ->
        let collection =
          [
            C.Item.make ~id:"0" ~value:"foo";
            C.Item.make ~id:"1" ~value:"foobar";
            C.Item.make ~id:"2" ~value:"bar";
            C.Item.make ~id:"3" ~value:"barfoo";
          ]
        in
        let module I = (val CM.make ()) in
        let result = I.(Completer.read this ~input:"foo" ~collection) in
        let expect =
          [
            C.Candidate.make ~start:0 ~length:3 ~value:(List.nth collection 0);
            C.Candidate.make ~start:0 ~length:3 ~value:(List.nth collection 1);
          ]
        in
        Alcotest.(check @@ list F.Testable.Completer.canditate) "completed" expect result);
  ]
