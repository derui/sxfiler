open Sxfiler_core
module D = Sxfiler_domain
module Tr = Sxfiler_translator
module G = Sxfiler_generated

let data =
  let history = D.Location_history.make ~max_record_num:D.Common.(Positive_number.make 100 |> Option.get) () in
  let record =
    D.Location_history.Record.make
      ~location:(Path.of_string "/root" |> Result.get_ok)
      ~timestamp:(Time.of_float 0. |> Option.get)
  in
  D.Location_history.add_record record history

let test_set =
  [
    Alcotest_lwt.test_case_sync "can translate to/from domain" `Quick (fun () ->
        Alcotest.(check @@ result (of_pp D.Location_history.pp) @@ of_pp Tr.Location_history.pp_error)
          "domain" (Ok data)
          Tr.Location_history.(to_domain @@ of_domain data));
    Alcotest_lwt.test_case_sync "return error when the record have invalid path" `Quick (fun () ->
        let data =
          {
            G.Filer.LocationHistory.records =
              [ { G.Filer.LocationRecord.location = ""; timestamp = "2020-02-03T00:00:00Z" } ];
            max_record_number = 100;
          }
        in
        Alcotest.(check @@ result (of_pp D.Location_history.pp) @@ of_pp Tr.Location_history.pp_error)
          "domain"
          (Error Tr.Location_history.(Invalid_path ""))
          Tr.Location_history.(to_domain data));
  ]
