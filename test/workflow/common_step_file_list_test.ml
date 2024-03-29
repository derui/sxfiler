open Sxfiler_domain
module C = Sxfiler_core
module P = Sxfiler_dependency
module S = Sxfiler_workflow.Common_step.File_list

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

let get_mock scan_location =
  (module struct
    let scan_location path = scan_location path |> Lwt.return
  end : S.Instance)

let scan_step_tests =
  [
    Alcotest_lwt.test_case "scan file list at the location with step" `Quick (fun _ () ->
        let list =
          File_list.(
            make ~id:(Id.make "test")
              ~location:(C.Path.of_string "/location" |> Result.get_ok)
              ~sort_type:Types.Sort_type.Name)
        in
        let%lwt scanned =
          S.scan list
          |> P.provide (function `Step_file_list_instance c -> P.Context.value (get_mock (fun _ -> Ok [ file_item ])) c)
          |> P.run
        in
        let items =
          match scanned with File_list.Valid { items; _ } -> items | No_location _ -> Alcotest.fail "not scanned"
        in
        Alcotest.(check @@ list @@ of_pp File_item.pp) "list of items" [ file_item ] items;
        Lwt.return_unit);
    Alcotest_lwt.test_case "get No_location when scan step returned error" `Quick (fun _ () ->
        let list =
          File_list.(
            make ~id:(Id.make "test")
              ~location:(C.Path.of_string "/location" |> Result.get_ok)
              ~sort_type:Types.Sort_type.Name)
        in
        let%lwt scanned =
          S.scan list
          |> P.provide (function `Step_file_list_instance c ->
                 P.Context.value (get_mock (fun path -> Error (`Not_exists path))) c)
          |> P.run
        in
        let is_no_location = match scanned with File_list.No_location _ -> true | Valid _ -> false in
        Alcotest.(check @@ bool) "no location" true is_no_location;
        Lwt.return_unit);
  ]

and reload_step_tests =
  let get_mock scan_location =
    (module struct
      let counter = ref 0

      let scan_location path =
        incr counter;
        scan_location !counter path |> Lwt.return
    end : S.Instance)
  in
  [
    Alcotest_lwt.test_case "reload scanned file list" `Quick (fun _ () ->
        let list =
          File_list.(
            make ~id:(Id.make "test")
              ~location:(C.Path.of_string "/location" |> Result.get_ok)
              ~sort_type:Types.Sort_type.Name)
        in
        let open P.Infix in
        let%lwt scanned =
          S.scan list >>= S.reload
          |> P.provide (function `Step_file_list_instance c ->
                 P.Context.value (get_mock (fun counter _ -> if counter = 0 then Ok [ file_item ] else Ok [])) c)
          |> P.run
        in

        (match scanned with
        | File_list.Valid { items; _ } -> Alcotest.(check @@ list @@ of_pp File_item.pp) "reloaded" [] items
        | No_location _                -> Alcotest.fail "not scanned");
        Lwt.return_unit);
    Alcotest_lwt.test_case "reload step returns No_location when current location not exists" `Quick (fun _ () ->
        let location = C.Path.of_string "/location" |> Result.get_ok in
        let list = File_list.make ~id:(File_list.Id.make "test") ~location ~sort_type:Types.Sort_type.Name in
        let scan_location counter path = if counter = 0 then Ok [ file_item ] else Error (`Not_exists path) in
        let open P.Infix in
        let%lwt scanned =
          S.scan list >>= S.reload
          |> P.provide (function `Step_file_list_instance c -> P.Context.value (get_mock scan_location) c)
          |> P.run
        in
        (match scanned with
        | File_list.Valid _ -> Alcotest.fail "invalid course"
        | No_location { id; location = location'; _ } ->
            Alcotest.(check @@ of_pp File_list.Id.pp) "reloaded" list.id id;
            Alcotest.(check @@ of_pp C.Path.pp) "path" location location');
        Lwt.return_unit);
  ]

let test_set = List.concat [ scan_step_tests; reload_step_tests ]
