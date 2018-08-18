open Sxfiler_core

module D = Sxfiler_domain
module Tr = Sxfiler_server_translator
module T = Sxfiler_rpc.Types

let data = {
  D.Scanner.id = "id";
  location = Path.of_string "/bar";
  nodes = [];
  history = D.Location_history.make ();
}

let testcases = [
  "can translate to/from domain", `Quick, (fun () ->
      let expected = {
        T.Scanner.id = "id";
        location = "/bar";
        nodes = [];
        history = Tr.Location_history.of_domain @@ D.Location_history.make ();
      } in
      Alcotest.(check @@ of_pp Fmt.nop) "domain" expected (Tr.Scanner.of_domain data)
    );
  "can translate to/from yojson", `Quick, (fun () ->
      let data = Tr.Scanner.of_domain data in
      Alcotest.(check @@ result (of_pp Fmt.nop) (of_pp Fmt.nop)) "yojson"
        (Ok data) (Tr.Scanner.of_yojson @@ Tr.Scanner.to_yojson data)
    );
]

let suite = [
  "scanner", testcases;
]
