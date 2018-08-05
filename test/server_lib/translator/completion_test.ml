open Sxfiler_core

module D = Sxfiler_completion.Domain
module T = Sxfiler_server_translator.Completion

let testcases = [
  "can translate item to/from domain", `Quick, (fun () ->
      let data = {D.Item.id = "value";
                  value = "foobar"
                 }
      in
      Alcotest.(check @@ of_pp Fmt.nop) "empty" data (T.Item.to_domain @@ T.Item.of_domain data)
    );
  "can translate candidate to/from domain", `Quick, (fun () ->
      let data = {D.Candidate.start = 1;
                  length = 20;
                  value = {D.Item.id = "foo"; value = "value"};
                 }
      in
      Alcotest.(check @@ of_pp Fmt.nop) "empty" data T.Candidate.(to_domain @@ of_domain data)
    );

  "can translate item to/from json", `Quick, (fun () ->
      let data = {T.Item.id = "value"; value = "foobar"} in
      Alcotest.(check @@ result (of_pp T.Item.pp) string) "yojson"
        (Ok data) T.Item.(of_yojson @@ to_yojson data)
    );
  "can translate candidate to/from json", `Quick, (fun () ->
      let data = {T.Candidate.start = 1;
                  length = 20;
                  value = {T.Item.id = "foo"; value = "value"};
                 }
      in
      Alcotest.(check @@ result (of_pp T.Candidate.pp) string) "yojson"
        (Ok data) T.Candidate.(of_yojson @@ to_yojson data)
    );

]

let suite = [
  "completion", testcases;
]
