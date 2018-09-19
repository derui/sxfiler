open Sxfiler_core
module I = Sxfiler_server_infra

let test_set =
  [ ( "get_node should return None if file is not found"
    , `Quick
    , fun () ->
      let module R = I.Location_scanner_service in
      Alcotest.(check @@ option @@ of_pp Fmt.nop)
        "Not found" None (R.get_node "parent" "not_found") )
  ; ( "get_node should return node if file found"
    , `Quick
    , fun () ->
      let module R = I.Location_scanner_service in
      Fun.bracket ~setup:(fun () -> Filename.temp_file "action" "tmp") ~teardown:Sys.remove
      @@ fun tempfile ->
      let oc = open_out tempfile in
      Unix.chmod tempfile 0o666 ;
      output_string oc "foo" ;
      close_out oc ;
      let node = R.get_node (Filename.dirname tempfile) (Filename.basename tempfile) in
      match node with
      | None -> Alcotest.fail "Not found "
      | Some node ->
        let module (* stat of file can not fix on unit test, so assert some record only. *)
          N =
          Sxfiler_domain.Node
        in
        let module S = Sxfiler_domain.File_stat in
        Alcotest.(check string) "full_path" tempfile @@ Path.to_string node.N.full_path ;
        Alcotest.(check @@ option string) "link_path" None node.N.link_path ;
        Alcotest.(check int32) "stat.mode" 0o666l node.N.stat.S.mode ;
        Alcotest.(check int64) "stat.size" 3L node.N.stat.S.size ;
        Alcotest.(check bool) "stat.is_directory" false node.N.stat.S.is_directory ;
        Alcotest.(check bool) "stat.is_file" true node.N.stat.S.is_file ;
        Alcotest.(check bool) "stat.is_symlink" false node.N.stat.S.is_symlink )
  ; ( "get_node can handle symlink as is"
    , `Quick
    , fun () ->
      let module R = I.Location_scanner_service in
      let linkname = "link" in
      Fun.bracket
        ~setup:(fun () -> linkname)
        ~teardown:(fun linkname -> Sys.remove Filename.(concat (get_temp_dir_name ()) linkname))
      @@ fun linkname ->
      Fun.bracket
        ~setup:(fun () -> Filename.temp_file "action" "tmp")
        ~teardown:Sys.remove
        (fun tempfile ->
           let dir = Filename.dirname tempfile in
           Unix.symlink tempfile Filename.(concat dir linkname) ;
           let node = R.get_node dir linkname in
           match node with
           | None -> Alcotest.fail "Not found "
           | Some node ->
             let module (* stat of file can not fix on unit test, so assert some record only. *)
               N =
               Sxfiler_domain.Node
             in
             let module S = Sxfiler_domain.File_stat in
             Alcotest.(check bool) "stat.is_symlink" true node.N.stat.S.is_symlink ) )
  ; ( "get_node can handle directory as is"
    , `Quick
    , fun () ->
      let temp_dir = ref "" in
      let tempfile = Filename.temp_file "action" "" in
      temp_dir := tempfile ;
      Sys.remove tempfile ;
      Fun.bracket
        ~setup:(fun _ -> Unix.mkdir !temp_dir 0o755)
        ~teardown:(fun _ -> Unix.rmdir !temp_dir)
      @@ fun () ->
      let dir = Filename.dirname !temp_dir and fname = Filename.basename !temp_dir in
      let node = I.Location_scanner_service.get_node dir fname in
      match node with
      | None -> Alcotest.fail "Not found "
      | Some node ->
        let module (* stat of file can not fix on unit test, so assert some record only. *)
          N =
          Sxfiler_domain.Node
        in
        let module S = Sxfiler_domain.File_stat in
        Alcotest.(check bool) "stat.is_file" false node.N.stat.S.is_file ;
        Alcotest.(check bool) "stat.is_directory" true node.N.stat.S.is_directory )
  ; Alcotest_lwt.test_case "find_by_dir with empty directory" `Quick (fun _ () ->
        let tempfile = Filename.temp_file "action" "" in
        Sys.remove tempfile ;
        Unix.mkdir tempfile 0o755 ;
        Lwt.finalize
          (fun () ->
             let path = Path.of_string tempfile in
             let%lwt nodes = I.Location_scanner_service.scan path in
             Alcotest.(check @@ list @@ of_pp Fmt.nop) "nodes" [] nodes ;
             Lwt.return_unit )
          (fun () -> Lwt.return @@ Unix.rmdir tempfile) )
  ; Alcotest_lwt.test_case "find_by_dir from directory that contains regular files" `Quick
      (fun _ () ->
         let module Dummy = struct
           let getcwd () = Sys.getcwd ()
         end in
         let to_path s = Path.resolve (module Dummy) @@ Path.of_string s in
         let path = to_path "./data_real/file_only" in
         let%lwt nodes = I.Location_scanner_service.scan path in
         let module N = Sxfiler_domain.Node in
         let nodes =
           List.map (fun v -> v.N.full_path) nodes |> List.map Path.to_string |> List.sort compare
         in
         Alcotest.(check @@ list string)
           "nodes"
           [ Path.to_string @@ to_path "./data_real/file_only/file1"
           ; Path.to_string @@ to_path "./data_real/file_only/file2" ]
           nodes ;
         Lwt.return_unit ) ]
