open Sxfiler_core
module D = Sxfiler_domain
module Tr = Sxfiler_translator

let test_set =
  let history = D.Location_history.make () in
  [
    Alcotest_lwt.test_case_sync "can translate to/from domain" `Quick (fun () ->
        let location = Path.of_string "/root" |> Result.get_ok in
        let list = D.File_list.make ~id:(D.File_list.Id.make "id") ~location ~sort_order:D.Types.Sort_type.Name in
        let item = Test_fixtures.File_item.fixture () in
        let list' = D.File_list.scan (`Scanned [ item ]) list in
        let left_file_window = D.File_window.make_left ~file_list:list' ~history in
        let right_file_window = D.File_window.make_right ~file_list:list' ~history in
        let filer = D.Filer.make ~left_file_window ~right_file_window |> Tr.Filer.of_domain in
        let expected_file_list =
          Sxfiler_generated.(
            Filer.FileList.
              {
                id = "id";
                location = "/root";
                items = [ Tr.File_item.of_domain item ];
                sort_order = Types.SortType.NAME;
              })
        in

        let expected =
          Sxfiler_generated.(
            Filer.Filer.
              {
                left_file_window =
                  Some
                    Filer.FileWindow.
                      { file_list = Some expected_file_list; history = Some Filer.LocationHistory.{ records = [] } };
                right_file_window =
                  Some
                    Filer.FileWindow.
                      { file_list = Some expected_file_list; history = Some Filer.LocationHistory.{ records = [] } };
              })
        in
        Alcotest.(check @@ of_pp Sxfiler_generated.Filer.Filer.pp) "domain" expected filer);
  ]
