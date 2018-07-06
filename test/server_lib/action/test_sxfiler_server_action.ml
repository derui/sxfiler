open Sxfiler_core
module A = Sxfiler_server_action

let file_operations = [
  "get_node should return None if file is not found", `Quick, (fun () ->
      Alcotest.(check @@ option @@ of_pp Fmt.nop) "Not found" None (A.File_op.get_node "parent" "not_found")
    );
  "get_node should return node if file found", `Quick, (fun () ->
      Fun.bracket ~setup:(fun () -> Filename.temp_file "action" "tmp")
        ~teardown:Sys.remove
      @@ fun tempfile ->
      let oc = open_out tempfile in
      Unix.chmod tempfile 0o666;
      output_string oc "foo";
      close_out oc;

      let node = A.File_op.get_node (Filename.dirname tempfile) (Filename.basename tempfile) in
      match node with
      | None -> Alcotest.fail "Not found "
      | Some node -> begin
          (* stat of file can not fix on unit test, so assert some record only. *)
          let module N = Sxfiler_types.Node in
          let module S = Sxfiler_types.File_stat in
          Alcotest.(check string) "full_path" tempfile node.N.full_path;
          Alcotest.(check string) "parent_directory" (Filename.dirname tempfile) node.N.parent_directory;
          Alcotest.(check @@ option string) "link_path" None node.N.link_path;
          Alcotest.(check int32) "stat.mode" 0o666l node.N.stat.S.mode;
          Alcotest.(check int64) "stat.size" 3L node.N.stat.S.size;
          Alcotest.(check bool) "stat.is_directory" false node.N.stat.S.is_directory;
          Alcotest.(check bool) "stat.is_file" true node.N.stat.S.is_file;
          Alcotest.(check bool) "stat.is_symlink" false node.N.stat.S.is_symlink;
        end
    );
  "get_node can handle symlink as is", `Quick, (fun () ->
      let linkname = "link" in
      Fun.bracket ~setup:(fun () -> linkname)
        ~teardown:(fun linkname -> Sys.remove Filename.(concat (get_temp_dir_name ()) linkname))
      @@ fun linkname ->
      Fun.bracket ~setup:(fun () -> Filename.temp_file "action" "tmp" )
        ~teardown:Sys.remove
        (fun tempfile ->
           let dir = Filename.dirname tempfile in
           Unix.symlink tempfile Filename.(concat dir linkname);

           let node = A.File_op.get_node dir linkname in
           match node with
           | None -> Alcotest.fail "Not found "
           | Some node -> begin
               (* stat of file can not fix on unit test, so assert some record only. *)
               let module N = Sxfiler_types.Node in
               let module S = Sxfiler_types.File_stat in
               Alcotest.(check bool) "stat.is_symlink" true node.N.stat.S.is_symlink;
             end
        )
    );
  "get_node can handle directory as is", `Quick, (fun ctx ->
      let temp_dir = ref "" in
      let tempfile = Filename.temp_file "action" "" in
      temp_dir := tempfile;
      Sys.remove tempfile;

      Fun.bracket ~setup:(fun _ -> Unix.mkdir !temp_dir 0o755) ~teardown:(fun _ -> Unix.rmdir !temp_dir)
      @@ fun () ->
      let dir = Filename.dirname !temp_dir
      and fname = Filename.basename !temp_dir in
      let node = A.File_op.get_node dir fname in
      match node with
      | None -> Alcotest.fail "Not found "
      | Some node -> begin
          (* stat of file can not fix on unit test, so assert some record only. *)
          let module N = Sxfiler_types.Node in
          let module S = Sxfiler_types.File_stat in
          Alcotest.(check bool) "stat.is_file" false node.N.stat.S.is_file;
          Alcotest.(check bool) "stat.is_directory" true node.N.stat.S.is_directory;
        end
    );
]

let real_test = [
  Alcotest_lwt.test_case "take_snapshot with empty directory" `Quick (fun switch () ->
      let tempfile = Filename.temp_file "action" "" in
      Sys.remove tempfile;
      Unix.mkdir tempfile 0o755;

      Lwt.finalize (fun () ->
          let module R = A.Real in
          let%lwt snapshot = R.No_side_effect.take_snapshot ~directory:tempfile in
          let module S = Sxfiler_types.Tree_snapshot in
          Alcotest.(check string) "directory" tempfile snapshot.S.directory;
          Alcotest.(check @@ list @@ of_pp Fmt.nop) "nodes" [] snapshot.S.nodes;
          Lwt.return_unit
        )
        (fun () -> Lwt.return @@ Unix.rmdir tempfile)
    );
  Alcotest_lwt.test_case "take_snapshot from directory that contains regular files" `Quick (fun switch () ->
      let module R = A.Real in
      let path = "./data_real/file_only" in
      let%lwt snapshot = R.No_side_effect.take_snapshot ~directory:path in
      let module S = Sxfiler_types.Tree_snapshot in
      let module N = Sxfiler_types.Node in
      Alcotest.(check string) "directory" path snapshot.S.directory;
      let nodes = List.sort compare @@ List.map (fun v -> v.N.full_path) snapshot.S.nodes in
      Alcotest.(check @@ list string) "nodes" [Filename.concat path "file1";
                                               Filename.concat path "file2"] nodes;
      Lwt.return_unit
    );
]

let () =
  Alcotest.run "Sxfiler server actions" [
    "file operations", file_operations;
    "real actions", real_test;
  ]
