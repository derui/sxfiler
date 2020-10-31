open Sxfiler_domain
module C = Sxfiler_core

let base_time = C.Time.of_float 2. |> Option.get

let stat time =
  let open File_stat in
  Stat.make ~mode:Mode.empty
    ~uid:(Uid.make 10 |> C.Result.get_ok)
    ~gid:(Gid.make 10 |> C.Result.get_ok)
    ~atime:time ~ctime:time ~mtime:time
    ~size:(Size.make 1L |> C.Result.get_ok)

let file_stat = stat base_time |> File_stat.make_file

let file_item =
  File_item.(make ~id:(Id.make "string") ~full_path:C.Path.(of_string "test.txt" |> Result.get_ok) ~stat:file_stat)

let base_tests =
  [
    Alcotest_lwt.test_case_sync "make list that in not scanned yet" `Quick (fun () ->
        let list =
          File_list.(
            make ~id:(Id.make "test")
              ~location:(C.Path.of_string "/location" |> Result.get_ok)
              ~sort_type:Types.Sort_type.Name)
        in
        Alcotest.(check @@ of_pp File_list.Id.pp) "empty" File_list.Id.(make "test") list.id);
    Alcotest_lwt.test_case_sync "find item in list" `Quick (fun () ->
        let list =
          File_list.(
            make ~id:(Id.make "test")
              ~location:(C.Path.of_string "/location" |> Result.get_ok)
              ~sort_type:Types.Sort_type.Name)
        in
        let scanned = File_list.scan (`Scanned [ file_item ]) list in
        Alcotest.(check @@ option Test_fixtures.Testable.file_item)
          "found" (Some file_item)
          (File_list.find_item ~id:(File_item.id file_item) scanned);
        Alcotest.(check @@ option Test_fixtures.Testable.file_item)
          "not found" None
          (File_list.find_item ~id:(File_item.Id.make "unknown") scanned));
  ]

and mark_tests =
  [
    Alcotest_lwt.test_case_sync "mark items in file list" `Quick (fun () ->
        let list =
          File_list.(
            make ~id:(Id.make "test")
              ~location:(C.Path.of_string "/location" |> Result.get_ok)
              ~sort_type:Types.Sort_type.Name)
        in
        let scanned = File_list.scan (`Scanned [ file_item ]) list in
        let scanned = File_list.mark_items ~ids:[ File_item.id file_item ] scanned in
        let items = File_list.marked_items scanned in
        Alcotest.(check @@ list @@ of_pp File_item.pp) "list of items" [ File_item.mark file_item ] items);
    Alcotest_lwt.test_case_sync "unmark items in file list" `Quick (fun () ->
        let list =
          File_list.(
            make ~id:(Id.make "test")
              ~location:(C.Path.of_string "/location" |> Result.get_ok)
              ~sort_type:Types.Sort_type.Name)
        in
        let scanned = File_list.scan (`Scanned [ file_item ]) list in
        let ids = [ File_item.id file_item ] in
        let scanned = File_list.mark_items ~ids scanned |> File_list.unmark_items ~ids in
        let items = File_list.marked_items scanned in
        Alcotest.(check @@ list @@ of_pp File_item.pp) "list of items" [] items);
    Alcotest_lwt.test_case_sync "ignore ids that are not found in file list" `Quick (fun () ->
        let list =
          File_list.(
            make ~id:(Id.make "test")
              ~location:(C.Path.of_string "/location" |> Result.get_ok)
              ~sort_type:Types.Sort_type.Name)
        in
        let scanned = File_list.scan (`Scanned [ file_item ]) list in
        let scanned = File_list.mark_items ~ids:[ File_item.Id.make "unknown" ] scanned in
        let items = File_list.marked_items scanned in
        Alcotest.(check @@ list @@ of_pp File_item.pp) "list of items" [] items);
  ]

and diff_tests =
  let file_item =
    File_item.(make ~id:(Id.make "string") ~full_path:C.Path.(of_string "test.txt" |> Result.get_ok) ~stat:file_stat)
  in
  let left_only_item =
    File_item.(make ~id:(Id.make "left") ~full_path:C.Path.(of_string "left.txt" |> Result.get_ok) ~stat:file_stat)
  in
  let right_only_item =
    File_item.(make ~id:(Id.make "right") ~full_path:C.Path.(of_string "right.txt" |> Result.get_ok) ~stat:file_stat)
  in

  [
    Alcotest_lwt.test_case_sync "no have difference file_lists that are no_location" `Quick (fun () ->
        let list =
          File_list.(
            make ~id:(Id.make "test")
              ~location:(C.Path.of_string "/location" |> Result.get_ok)
              ~sort_type:Types.Sort_type.Name)
        in
        let left = File_list.scan `No_location list in
        let right = File_list.scan `No_location list in

        let items = File_list.diff ~left ~right in
        Alcotest.(check @@ list @@ of_pp Fmt.nop) "difference" [] items);
    Alcotest_lwt.test_case_sync "only left side items when right is no location" `Quick (fun () ->
        let list =
          File_list.(
            make ~id:(Id.make "test")
              ~location:(C.Path.of_string "/location" |> Result.get_ok)
              ~sort_type:Types.Sort_type.Name)
        in
        let left = File_list.scan (`Scanned [ file_item ]) list in
        let right = File_list.scan `No_location list in

        let items = File_list.diff ~left ~right in
        Alcotest.(check @@ list @@ of_pp Fmt.nop) "difference" [ `In_left file_item ] items);
    Alcotest_lwt.test_case_sync "only right side items when left is no location" `Quick (fun () ->
        let list =
          File_list.(
            make ~id:(Id.make "test")
              ~location:(C.Path.of_string "/location" |> Result.get_ok)
              ~sort_type:Types.Sort_type.Name)
        in
        let left = File_list.scan (`Scanned [ file_item; left_only_item ]) list in
        let right = File_list.scan (`Scanned [ file_item; right_only_item ]) list in

        let items = File_list.diff ~left ~right in
        Alcotest.(check @@ list @@ of_pp Fmt.nop)
          "difference"
          [ `In_left left_only_item; `In_right right_only_item ]
          items);
  ]

let test_set = List.concat [ base_tests; mark_tests; diff_tests ]
