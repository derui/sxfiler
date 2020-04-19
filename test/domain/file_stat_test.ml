module C = Sxfiler_core
open Sxfiler_domain

let test_set =
  let stat =
    let open File_stat in
    let time = C.Time.of_float 1. |> Option.get in
    Stat.make ~mode:Mode.empty
      ~uid:(Uid.make 10 |> C.Result.get_ok)
      ~gid:(Gid.make 10 |> C.Result.get_ok)
      ~atime:time ~ctime:time ~mtime:time
      ~size:(Size.make 1L |> C.Result.get_ok)
  in
  [
    Alcotest_lwt.test_case_sync "create stat for file" `Quick (fun () ->
        let stat = File_stat.make_file stat in
        Alcotest.(check @@ of_pp Fmt.nop) "kind" File_stat.Kind.File stat.kind);
    Alcotest_lwt.test_case_sync "create stat for directory" `Quick (fun () ->
        let stat = File_stat.make_directory stat in
        Alcotest.(check @@ of_pp Fmt.nop) "kind" File_stat.Kind.Directory stat.kind);
    Alcotest_lwt.test_case_sync "create stat for symlink" `Quick (fun () ->
        let path = C.Path.of_string "/foo" |> Result.get_ok in
        let stat = File_stat.make_symlink ~stat ~link_path:path in
        Alcotest.(check @@ of_pp Fmt.nop) "kind" (File_stat.Kind.Symlink path) stat.kind);
    Alcotest_lwt.test_case_sync "create capability" `Quick (fun () ->
        let cap = File_stat.Capability.make ~writable:true ~readable:false ~executable:false in
        Alcotest.(check bool) "writable" true cap.writable;
        Alcotest.(check bool) "readable" false cap.readable;
        Alcotest.(check bool) "executable" false cap.executable);
    Alcotest_lwt.test_case_sync "uid must be greater equal 0" `Quick (fun () ->
        let uid = File_stat.Uid.make (-1) in
        Alcotest.(check @@ result (of_pp File_stat.Uid.pp) string) "error" (Error "uid must be greater or equal 0") uid);
    Alcotest_lwt.test_case_sync "gid must be greater equal 0" `Quick (fun () ->
        let gid = File_stat.Gid.make (-1) in
        Alcotest.(check @@ result (of_pp File_stat.Gid.pp) string) "error" (Error "gid must be greater or equal 0") gid);
    Alcotest_lwt.test_case_sync "size must be greater equal 0" `Quick (fun () ->
        let size = File_stat.Size.make (-1L) in
        Alcotest.(check @@ result (of_pp File_stat.Size.pp) string)
          "error" (Error "Size must be greater or equal 0") size);
  ]
