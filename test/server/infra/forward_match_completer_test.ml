module C = Sxfiler_domain.Completion
module CM = Sxfiler_server_infra.Forward_match_completer

let test_set =
  [
    ( "can return candidates from collection by forward match",
      `Quick,
      fun () ->
        let collection =
          [
            { C.Item.value = "foo"; id = "0" };
            { C.Item.value = "foobar"; id = "1" };
            { C.Item.value = "bar"; id = "2" };
            { C.Item.value = "barfoo"; id = "3" };
          ]
        in
        let module I = (val CM.make ()) in
        let result = I.(Completer.read this ~input:"foo" ~collection) in
        let expect =
          [
            { C.Candidate.start = 0; length = 3; value = List.nth collection 0 };
            { C.Candidate.start = 0; length = 3; value = List.nth collection 1 };
          ]
        in
        Alcotest.(check @@ list (of_pp C.Candidate.pp)) "completed" expect result );
  ]
