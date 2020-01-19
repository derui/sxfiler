open Sxfiler_core
module I = Sxfiler_server_infra

let test_set =
  [
    Alcotest_lwt.test_case "find_by_dir with empty directory" `Quick (fun _ () ->
        let tempfile = Filename.temp_file "action" "" in
        Sys.remove tempfile;
        Unix.mkdir tempfile 0o755;
        Lwt.finalize
          (fun () ->
            let path = Path.of_string tempfile in
            let%lwt file_list = I.Location_scanner_service.scan path in
            Alcotest.(check @@ list @@ Test_fixtures.Testable.file_item) "nodes" [] file_list.items;
            Lwt.return_unit)
          (fun () -> Lwt.return @@ Unix.rmdir tempfile));
    Alcotest_lwt.test_case "scan from directory that contains regular files" `Quick (fun _ () ->
        let module Dummy = struct
          let getcwd () = Sys.getcwd ()
        end in
        let to_path s = Path.resolve (module Dummy) @@ Path.of_string s in
        let path = to_path "./data_real/file_only" in
        let%lwt file_list = I.Location_scanner_service.scan path in
        let module N = Sxfiler_domain.File_item in
        let nodes =
          List.map (fun v -> v.N.full_path) file_list.items
          |> List.map Path.to_string |> List.sort compare
        in
        Alcotest.(check @@ list string)
          "nodes"
          [
            Path.to_string @@ to_path "./data_real/file_only/file1";
            Path.to_string @@ to_path "./data_real/file_only/file2";
          ]
          nodes;
        Lwt.return_unit);
  ]
