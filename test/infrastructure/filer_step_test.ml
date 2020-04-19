open Sxfiler_core
module I = Sxfiler_infrastructure
module F = Sxfiler_workflow

let copy_item_test =
  [
    Alcotest_lwt.test_case "copy a item to destination" `Quick (fun _ () ->
        let temp_dir = File.mk_temp_dir "nts" in
        let dest = File.mk_temp_dir "destination" in
        let temp_file = Filename.temp_file ~temp_dir "item" "sample" in
        let finalizer () =
          File.remove temp_dir;
          File.remove dest;
          Lwt.return_unit
        in
        finalizer
        |> Lwt.finalize (fun () ->
               let%lwt ret =
                 I.Filer_step.copy_item
                   {
                     source = Path.of_string temp_file |> Result.get_ok;
                     dest = Path.of_list [ dest; Filename.basename temp_file ] |> Result.get_ok;
                     overwrite = false;
                   }
               in
               let dest_file = Filename.basename temp_file |> Filename.concat dest in
               Alcotest.(check @@ result unit @@ of_pp F.Common_step.Filer.pp_operation_error) "result" (Ok ()) ret;
               Alcotest.(check bool) "destination" true (Sys.file_exists dest_file);
               Alcotest.(check bool) "source" true (Sys.file_exists temp_file);
               Lwt.return_unit));
    Alcotest_lwt.test_case "raise error when target is not found" `Quick (fun _ () ->
        let temp_dir = File.mk_temp_dir "nts" in
        let dest = File.mk_temp_dir "destination" in
        let temp_file = Filename.temp_file ~temp_dir "item" "sample" in
        let finalizer () =
          File.remove temp_dir;
          File.remove dest;
          Lwt.return_unit
        in
        finalizer
        |> Lwt.finalize (fun () ->
               let%lwt ret =
                 I.Filer_step.copy_item
                   {
                     source = Path.of_string temp_file |> Result.get_ok;
                     dest = Path.of_list [ dest ^ "_ignore"; Filename.basename temp_file ] |> Result.get_ok;
                     overwrite = false;
                   }
               in
               let expected = Path.of_list [ dest ^ "_ignore"; Filename.basename temp_file ] |> Result.get_ok in
               Alcotest.(check @@ result unit @@ of_pp F.Common_step.Filer.pp_operation_error)
                 "ret" (Error (F.Common_step.Filer.Not_exists expected)) ret;
               Lwt.return_unit));
  ]

and move_item_test =
  [
    Alcotest_lwt.test_case "transport a item to destination" `Quick (fun _ () ->
        let temp_dir = File.mk_temp_dir "nts" in
        let dest = File.mk_temp_dir "destination" in
        let temp_file = Filename.temp_file ~temp_dir "item" "sample" in
        let finalizer () =
          File.remove temp_dir;
          File.remove dest;
          Lwt.return_unit
        in
        finalizer
        |> Lwt.finalize (fun () ->
               let%lwt _ =
                 I.Filer_step.move_item
                   {
                     source = Path.of_string temp_file |> Result.get_ok;
                     dest = Path.of_list [ dest; Filename.basename temp_file ] |> Result.get_ok;
                     overwrite = false;
                   }
               in
               let dest_file = Filename.basename temp_file |> Filename.concat dest in
               Alcotest.(check bool) "destination" true (Sys.file_exists dest_file);
               Alcotest.(check bool) "source" false (Sys.file_exists temp_file);
               Lwt.return_unit));
    Alcotest_lwt.test_case "transport items to destination another name" `Quick (fun _ () ->
        let temp_dir = File.mk_temp_dir "nts" in
        let dest = File.mk_temp_dir "destination" in
        let temp_file = Filename.temp_file ~temp_dir "item" "sample" in
        let dest' = Path.(of_list [ dest; "renamed" ]) |> Result.get_ok in
        let finalizer () =
          File.remove temp_dir;
          File.remove dest;
          Lwt.return_unit
        in
        finalizer
        |> Lwt.finalize (fun () ->
               let%lwt ret =
                 I.Filer_step.move_item
                   { source = Path.of_string temp_file |> Result.get_ok; dest = dest'; overwrite = false }
               in
               let dest_file = Filename.concat dest "renamed" in
               Alcotest.(check @@ result unit @@ of_pp F.Common_step.Filer.pp_operation_error) "ret" (Ok ()) ret;
               Alcotest.(check bool) "destination" true (Sys.file_exists dest_file);
               Alcotest.(check bool) "source" false (Sys.file_exists temp_file);
               Lwt.return_unit));
  ]

and scan_location_test =
  [
    Alcotest_lwt.test_case "find_by_dir with empty directory" `Quick (fun _ () ->
        let tempfile = Filename.temp_file "action" "" in
        Sys.remove tempfile;
        Unix.mkdir tempfile 0o755;
        Lwt.finalize
          (fun () ->
            let path = Path.of_string tempfile |> Result.get_ok in
            let%lwt items = I.Filer_step.scan_location path in
            Alcotest.(check @@ result (list Test_fixtures.Testable.file_item) (of_pp Fmt.nop)) "nodes" (Ok []) items;
            Lwt.return_unit)
          (fun () -> Lwt.return @@ Unix.rmdir tempfile));
    Alcotest_lwt.test_case "scan from directory that contains regular files" `Quick (fun _ () ->
        let module Dummy = struct
          let getcwd () = Sys.getcwd ()
        end in
        let to_path s = Path.of_string s |> Result.map @@ Path.resolve (module Dummy) |> Result.get_ok in
        let path = to_path "./data_real/file_only" in
        let%lwt items = I.Filer_step.scan_location path in
        let items = Result.get_ok items in
        let module N = Sxfiler_domain.File_item in
        let nodes = items |> List.map (N.item %> N.Item.full_path) |> List.map Path.to_string |> List.sort compare in
        Alcotest.(check @@ list string)
          "nodes"
          [
            Path.to_string @@ to_path "./data_real/file_only/file1";
            Path.to_string @@ to_path "./data_real/file_only/file2";
          ]
          nodes;
        Lwt.return_unit);
  ]

and delete_item_test =
  [
    Alcotest_lwt.test_case "delete an item" `Quick (fun _ () ->
        let temp_dir = File.mk_temp_dir "nts" in
        let temp_file = Filename.temp_file ~temp_dir "item" "sample" in
        let finalizer () =
          File.remove temp_dir;
          Lwt.return_unit
        in
        finalizer
        |> Lwt.finalize (fun () ->
               let%lwt items = I.Filer_step.scan_location (Path.of_string temp_dir |> Result.get_ok) in
               let%lwt ret =
                 match items with
                 | Ok [ item ]    -> I.Filer_step.delete_item item
                 | Ok _ | Error _ -> Alcotest.fail "illegal path"
               in

               Alcotest.(check @@ result unit @@ of_pp Fmt.nop) "result" (Ok ()) ret;
               Alcotest.(check bool) "source" false (Sys.file_exists temp_file);
               Lwt.return_unit));
  ]

let test_set = List.concat [ copy_item_test; move_item_test; scan_location_test; delete_item_test ]
