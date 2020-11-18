open Sxfiler_domain
module C = Sxfiler_core
module F = Test_fixtures

let test_set =
  let list_items = [ F.File_item.fixture (); F.File_item.fixture () ] in

  let file_list =
    File_list.(
      make ~id:(Id.make "left") ~location:(C.Path.of_string "/left" |> Result.get_ok) ~sort_type:Types.Sort_type.Name)
  in

  let file_list = file_list |> File_list.scan (`Scanned list_items) in

  [
    Alcotest_lwt.test_case_sync "reload only list" `Quick (fun () ->
        let window = File_window.make_left ~file_list ~history:(Location_history.make ()) in
        let list' = File_list.reload `No_location file_list in
        let window' = File_window.reload_list list' window |> C.Result.get_ok in
        Alcotest.(check @@ F.Testable.file_list_scanned) "updated" list' window'.file_list);
    Alcotest_lwt.test_case_sync "should not allow to reload if locations are not same" `Quick (fun () ->
        let window = File_window.make_left ~file_list ~history:(Location_history.make ()) in
        let new_location = C.Path.of_string "/root" |> Result.get_ok in
        let list' = File_list.change_location ~location:new_location file_list |> File_list.scan `No_location in
        let window' = File_window.reload_list list' window in
        Alcotest.(check @@ result (of_pp Fmt.nop) (of_pp Fmt.nop)) "updated" (Error `Not_same) window');
    Alcotest_lwt.test_case_sync "change location" `Quick (fun () ->
        let window = File_window.make_left ~file_list ~history:(Location_history.make ()) in
        let new_location = C.Path.of_string "/root" |> Result.get_ok in
        let file_list' =
          File_list.(make ~id:(Id.make "left") ~location:new_location ~sort_type:Types.Sort_type.Name)
          |> File_list.scan (`Scanned [])
        in
        let timestamp = C.Time.of_float 0. |> Option.get in
        let record = Location_history.Record.make ~location:new_location ~timestamp in
        let window' = File_window.move_location ~file_list:file_list' ~timestamp window |> C.Result.get_ok in

        Alcotest.(check @@ F.Testable.file_list_scanned) "updated" file_list' window'.file_list;
        Alcotest.(check @@ list F.Testable.History.record) "record" [ record ] window'.history.records);
    Alcotest_lwt.test_case_sync "should not allow to change location if it same" `Quick (fun () ->
        let window = File_window.make_left ~file_list ~history:(Location_history.make ()) in
        let timestamp = C.Time.of_float 0. |> Option.get in
        let window' = File_window.move_location ~file_list ~timestamp window in
        Alcotest.(check @@ result (of_pp Fmt.nop) @@ of_pp Fmt.nop) "can not change" (Error `Same) window');
  ]
