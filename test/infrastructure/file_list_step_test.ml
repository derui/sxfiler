open Sxfiler_core
module D = Sxfiler_domain
module I = Sxfiler_infrastructure
module F = Sxfiler_workflow

let scan_location_test =
  [
    Alcotest_lwt.test_case "find_by_dir with empty directory" `Quick (fun _ () ->
        let tempfile = Filename.temp_file "action" "" in
        Sys.remove tempfile;
        Unix.mkdir tempfile 0o755;
        Lwt.finalize
          (fun () ->
            let path = Path.of_string tempfile |> Result.get_ok in
            let%lwt items = I.File_list_step.Instance.scan_location path in
            Alcotest.(check @@ result (list Test_fixtures.Testable.file_item) (of_pp Fmt.nop)) "nodes" (Ok []) items;
            Lwt.return_unit)
          (fun () -> Lwt.return @@ Unix.rmdir tempfile));
    Alcotest_lwt.test_case "scan from directory that contains regular files" `Quick (fun _ () ->
        let module Dummy = struct
          let getcwd () = Sys.getcwd ()
        end in
        let to_path s = Path.of_string s |> Result.map @@ Path.resolve (module Dummy) |> Result.get_ok in
        let path = to_path "./data_real/file_only" in
        let%lwt items = I.File_list_step.Instance.scan_location path in
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

let test_set = scan_location_test
