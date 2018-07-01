open OUnit2
module A = Sxfiler_server_action

let suite = "Sxfiler server actions" >::: [
    "File operations" >::: [
      "get_node should return None if file is not found" >:: (fun _ ->
          assert_equal None (A.File_op.get_node "parent" "not_found")
        );
      "get_node should return node if file found" >:: (fun ctx ->
          let tempfile, oc = bracket_tmpfile ~prefix:"action" ~mode:[Pervasives.Open_text] ctx in
          Unix.chmod tempfile 0o666;
          output_string oc "foo";
          flush oc;

          let node = A.File_op.get_node (Filename.dirname tempfile) (Filename.basename tempfile) in
          match node with
          | None -> assert_failure "Not found "
          | Some node -> begin
              (* stat of file can not fix on unit test, so assert some record only. *)
              let module N = Sxfiler_types.Node in
              let module S = Sxfiler_types.File_stat in
              assert_equal ~msg:"full_path" tempfile node.N.full_path;
              assert_equal ~msg:"parent_directory" (Filename.dirname tempfile) node.N.parent_directory;
              assert_equal ~msg:"link_path" None node.N.link_path;
              assert_equal ~msg:"stat.mode" 0o666l node.N.stat.S.mode;
              assert_equal ~msg:"stat.size" 3L node.N.stat.S.size;
              assert_equal ~msg:"stat.is_directory" false node.N.stat.S.is_directory;
              assert_equal ~msg:"stat.is_file" true node.N.stat.S.is_file;
              assert_equal ~msg:"stat.is_symlink" false node.N.stat.S.is_symlink;
            end
        );
      "get_node can handle symlink as is" >:: (fun ctx ->
          let linkname = bracket (fun _ -> "link")
              (fun linkname _ -> Sys.remove Filename.(concat (get_temp_dir_name ()) linkname)) ctx in
          let tempfile, oc = bracket_tmpfile ~prefix:"action" ~mode:[Pervasives.Open_text] ctx in
          let dir = Filename.dirname tempfile in
          Unix.symlink tempfile Filename.(concat dir linkname);

          let node = A.File_op.get_node dir linkname in
          match node with
          | None -> assert_failure "Not found "
          | Some node -> begin
              (* stat of file can not fix on unit test, so assert some record only. *)
              let module N = Sxfiler_types.Node in
              let module S = Sxfiler_types.File_stat in
              assert_equal ~msg:"stat.is_symlink" true node.N.stat.S.is_symlink;
            end
        );
      "get_node can handle directory as is" >:: (fun ctx ->
          let temp_dir = ref "" in
          let tempfile, oc = bracket_tmpfile ctx in
          temp_dir := tempfile;
          Sys.remove tempfile;

          bracket (fun _ -> Unix.mkdir !temp_dir 0o755) (fun _ _ -> Unix.rmdir !temp_dir) ctx;
          let dir = Filename.dirname !temp_dir
          and fname = Filename.basename !temp_dir in
          let node = A.File_op.get_node dir fname in
          match node with
          | None -> assert_failure "Not found "
          | Some node -> begin
              (* stat of file can not fix on unit test, so assert some record only. *)
              let module N = Sxfiler_types.Node in
              let module S = Sxfiler_types.File_stat in
              assert_equal ~msg:"stat.is_file" false node.N.stat.S.is_file;
              assert_equal ~msg:"stat.is_directory" true node.N.stat.S.is_directory;
            end
        );
    ]
  ]


let () =
  run_test_tt_main suite |> ignore
