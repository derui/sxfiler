open Sxfiler_core
module D = Sxfiler_domain
module Tr = Sxfiler_translator

let test_set =
  [
    Alcotest_lwt.test_case_sync "can translate to/from domain" `Quick (fun () ->
        let list =
          D.File_list.make ~id:(D.File_list.Id.make "id")
            ~location:(Path.of_string "/root" |> Result.get_ok)
            ~sort_order:D.Types.Sort_type.Name
        in
        let item = Test_fixtures.File_item.fixture () in
        let list' = D.File_list.scan (`Scanned [ item ]) list in
        let list' = Tr.File_list.of_domain list' in
        let expected =
          Sxfiler_generated.(
            Filer.FileList.
              {
                id = "id";
                location = "/root";
                items = [ Tr.File_item.of_domain item ];
                sort_order = Types.SortType.NAME;
              })
        in
        Alcotest.(check @@ of_pp Sxfiler_generated.Filer.FileList.pp) "domain" expected list');
  ]
