open Sxfiler_core

module D = Sxfiler_domain
module T = Sxfiler_server_translator.Location_history

let data =
  let data' = D.Location_history.make () in
  let record = {
    D.Location_record.location = Path.of_string" /foo";
    timestamp = Int64.max_int;
  } in
  D.Location_history.add_record ~record data'

let testcases = [
  "can translate to/from domain", `Quick, (fun () ->
      Alcotest.(check @@ of_pp Fmt.nop) "domain" data (T.to_domain @@ T.of_domain data)
    );
  "can translate to/from yojson", `Quick, (fun () ->
      let data = T.of_domain data in
      Alcotest.(check @@ result (of_pp Fmt.nop) (of_pp Fmt.nop)) "yojson"
        (Ok data) (T.of_yojson @@ T.to_yojson data)
    );
]

let suite = [
  "location history", testcases;
]
