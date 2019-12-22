module D = Sxfiler_domain.Completion
module Tr = Sxfiler_server_translator.Completion
module Gen = Sxfiler_server_generated.Completion

let test_candidates =
  [
    ( "can translate candidates to/from domain",
      `Quick,
      fun () ->
        let data =
          [ { D.Candidate.start = 1; length = 20; value = { D.Item.id = "foo"; value = "value" } } ]
        in
        Alcotest.(check @@ of_pp Fmt.nop) "empty" data Tr.Candidates.(to_domain @@ of_domain data)
    );
  ]

let test_collection =
  [
    ( "can translate collection to/from domain",
      `Quick,
      fun () ->
        let data = [ { D.Item.id = "foo"; value = "value" } ] in
        Alcotest.(check @@ of_pp Fmt.nop) "empty" data Tr.Collection.(to_domain @@ of_domain data)
    );
  ]

let test_set = test_candidates @ test_collection
