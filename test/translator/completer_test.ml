module D = Sxfiler_domain.Completer
module Tr = Sxfiler_translator.Completer
module Gen = Sxfiler_generated.Completer
module F = Test_fixtures

let test_candidates =
  [
    Alcotest_lwt.test_case_sync "can translate candidates to/from domain" `Quick (fun () ->
        let data = [ D.Candidate.make ~start:1 ~length:20 ~value:(D.Item.make ~id:"foo" ~value:"value") ] in
        let actual =
          let v = Tr.Candidates.of_domain data in
          Tr.Candidates.to_domain v
        in
        Alcotest.(check @@ result (of_pp Fmt.nop) (of_pp Fmt.nop)) "empty" (Ok data) actual);
  ]

let test_collection =
  [
    Alcotest_lwt.test_case_sync "can translate collection to/from domain" `Quick (fun () ->
        let data = [ D.Item.make ~id:"foo" ~value:"value" ] in
        Alcotest.(check @@ result (list F.Testable.Completer.item) @@ of_pp Fmt.nop)
          "empty" (Ok data)
          Tr.Collection.(to_domain @@ of_domain data));
  ]

let test_set = List.concat [ test_candidates; test_collection ]
