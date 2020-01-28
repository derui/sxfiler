open Sxfiler_domain
module C = Sxfiler_core

let test_set =
  let base_time = C.Time.of_float 2. |> Option.get in
  let earlier_time = C.Time.of_float 1.5 |> Option.get in
  let stat time =
    let open File_stat in
    Stat.make ~mode:Mode.empty
      ~uid:(Uid.make 10 |> C.Result.get_ok)
      ~gid:(Gid.make 10 |> C.Result.get_ok)
      ~atime:time ~ctime:time ~mtime:time
      ~size:(Size.make 1L |> C.Result.get_ok)
  in
  let file_stat = stat base_time |> File_stat.make_file
  and directory_stat = stat base_time |> File_stat.make_directory
  and symlink_stat =
    stat base_time |> fun stat -> File_stat.make_symlink ~stat ~link_path:(C.Path.of_string "/link" |> Result.get_ok)
  in
  [
    Alcotest_lwt.test_case_sync "item must be unmarked that is created" `Quick (fun () ->
        let item =
          File_item.make ~id:(File_item.Id.make "id") ~stat:file_stat
            ~full_path:(C.Path.of_string "/root" |> Result.get_ok)
        in
        let ret = match item with File_item.Marked _ -> true | Unmarked _ -> false in
        Alcotest.(check bool) "unmarked" false ret);
    Alcotest_lwt.test_case_sync "can check what item is file" `Quick (fun () ->
        let item =
          File_item.make ~id:(File_item.Id.make "id") ~stat:file_stat
            ~full_path:(C.Path.of_string "/root" |> Result.get_ok)
        in
        Alcotest.(check bool) "file" true (File_item.is_file item));
    Alcotest_lwt.test_case_sync "can check what item is directory" `Quick (fun () ->
        let item =
          File_item.make ~id:(File_item.Id.make "id") ~stat:directory_stat
            ~full_path:(C.Path.of_string "/root" |> Result.get_ok)
        in
        Alcotest.(check bool) "directory" true (File_item.is_directory item));
    Alcotest_lwt.test_case_sync "can check what item is symlink" `Quick (fun () ->
        let item =
          File_item.make ~id:(File_item.Id.make "id") ~stat:symlink_stat
            ~full_path:(C.Path.of_string "/root" |> Result.get_ok)
        in
        Alcotest.(check bool) "directory" true (File_item.is_symlink item));
    Alcotest_lwt.test_case_sync "should be able to unwrap item" `Quick (fun () ->
        let item' =
          File_item.make ~id:(File_item.Id.make "id") ~stat:file_stat
            ~full_path:(C.Path.of_string "/root" |> Result.get_ok)
        in
        let marked = File_item.mark item' in
        Alcotest.(check string) "id" "id" File_item.(id item' |> Id.value);
        Alcotest.(check string) "id" "id" File_item.(id marked |> Id.value));
    Alcotest_lwt.test_case_sync "should not do nothing mark twice" `Quick (fun () ->
        let item =
          File_item.make ~id:(File_item.Id.make "id") ~stat:file_stat
            ~full_path:(C.Path.of_string "/root" |> Result.get_ok)
        in
        let marked = File_item.mark item in
        let twice_marked = File_item.mark marked in
        Alcotest.(check @@ of_pp File_item.pp) "marked" marked twice_marked);
    Alcotest_lwt.test_case_sync "should not do nothing unmark twice" `Quick (fun () ->
        let item =
          File_item.make ~id:(File_item.Id.make "id") ~stat:file_stat
            ~full_path:(C.Path.of_string "/root" |> Result.get_ok)
        in
        let unmarked = File_item.unmark item in
        let twice_unmarked = File_item.unmark unmarked in
        Alcotest.(check @@ of_pp File_item.pp) "unmark" unmarked twice_unmarked);
    Alcotest_lwt.test_case_sync "get compare function by name" `Quick (fun () ->
        let item1 =
          File_item.make ~id:(File_item.Id.make "id1") ~stat:file_stat
            ~full_path:(C.Path.of_string "/root1" |> Result.get_ok)
        in
        let item2 =
          File_item.make ~id:(File_item.Id.make "id2") ~stat:file_stat
            ~full_path:(C.Path.of_string "/root2" |> Result.get_ok)
        in
        let compare = File_item.compare_by Types.Sort_type.Name in
        Alcotest.(check int) "compare by name" (-1) (compare item1 item2));
    Alcotest_lwt.test_case_sync "get compare function by time" `Quick (fun () ->
        let item1 =
          File_item.(
            make ~id:(Id.make "id1")
              ~stat:(stat earlier_time |> File_stat.make_file)
              ~full_path:(C.Path.of_string "/root1" |> Result.get_ok))
        in
        let item2 =
          File_item.(
            make ~id:(Id.make "id2")
              ~stat:(stat base_time |> File_stat.make_file)
              ~full_path:(C.Path.of_string "/root2" |> Result.get_ok))
        in
        let compare = File_item.compare_by Types.Sort_type.Date in
        Alcotest.(check int) "compare by name" 1 (compare item2 item1));
    Alcotest_lwt.test_case_sync "get compare function by name" `Quick (fun () ->
        let item1 =
          File_item.(make ~id:(Id.make "id1") ~stat:file_stat ~full_path:(C.Path.of_string "/root1" |> Result.get_ok))
        in
        let item2 =
          File_item.(make ~id:(Id.make "id2") ~stat:file_stat ~full_path:(C.Path.of_string "/root2" |> Result.get_ok))
        in
        let compare = File_item.compare_by Types.Sort_type.Name in
        Alcotest.(check int) "compare by name" 1 (compare item2 item1));
  ]
