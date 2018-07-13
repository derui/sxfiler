open Sxfiler_core

let option_tests = [
  "detect none", `Quick, (fun () ->
      Alcotest.(check bool) "none is true" true (Option.is_none None);
      Alcotest.(check bool) "some is false" false (Option.is_none (Some 2))
    );
  "detect some", `Quick, (fun () ->
      Alcotest.(check bool) "none is true" true (Option.is_some (Some 1));
      Alcotest.(check bool) "none is false" false (Option.is_some None)
    );

  "get value from option with some", `Quick, (fun () ->
      Alcotest.(check int) "some" 1 (Option.get_exn (Some 1))
    );
  "raise exception when None", `Quick, (fun () ->
      Alcotest.check_raises "none" Option.Not_some (fun () -> Option.get_exn None)
    );

  "get value from option with some", `Quick, (fun () ->
      Alcotest.(check int) "some" 1 @@ Option.get ~default:(fun () -> 100) (Some 1)
    );
  "get default value", `Quick, (fun () ->
      Alcotest.(check int) "none" 100 @@ Option.get ~default:(fun () -> 100) None
    );
  "get Some with some", `Quick, (fun () ->
      Alcotest.(check @@ option int) "option" (Some 10) @@ Option.some 10
    );
]

let path_tests = [
  "separator of path on unix", `Quick, (fun () ->
      let module S = struct
        let getcwd () = "/foo"
        end in
      let expected = "/foo/a/b" in
      Alcotest.(check string) "unix" expected @@
      Path.(to_string ~env:`Unix @@ of_string ~env:`Unix (module S) "a/b")
    );
  "separator of path on windows", `Quick, (fun () ->
      let module S = struct
        let getcwd () = "\\foo"
        end in
      let expected = "\\foo\\a\\b" in
      Alcotest.(check string) "win" expected @@
      Path.(to_string ~env:`Win @@ of_string ~env:`Win (module S) "a\\b")
    );

  "ignore cwd if path is absolute", `Quick, (fun () ->
      let module S = struct
        let getcwd () = "/foo"
        end in
      let expected = "/bar/a" in
      Alcotest.(check string) "absolute" expected @@
      Path.(to_string ~env:`Unix @@ of_string ~env:`Unix (module S) "/bar/a")
    );

  "allow to use windows device name", `Quick, (fun () ->
      let module S = struct
        let getcwd () = "c:\\foo"
        end in
      let expected = "c:\\foo\\a" in
      Alcotest.(check string) "absolute" expected @@
      Path.(to_string ~env:`Win @@ of_string ~env:`Win (module S) "a")
    );

  "allow to contain . and ..", `Quick, (fun () ->
      let module S = struct
        let getcwd () = "/var"
        end in
      let expected = "/var/.././foo/../bar" in
      Alcotest.(check string) "absolute" expected @@
      Path.(to_string ~env:`Unix @@ of_string ~env:`Unix (module S) ".././foo/../bar")
    );

  "resolve current and parent directory to realpath", `Quick, (fun () ->
      let module S = struct
        let getcwd () = "/var"
        end in
      let expected = "/bar" in
      Alcotest.(check string) "realpath" expected @@
      Path.(to_string ~env:`Unix @@ resolve @@ of_string ~env:`Unix (module S) ".././foo/../bar")
    );

  "raise Empth_path", `Quick, (fun () ->
      let module S = struct
        let getcwd () = "/var"
        end in
      Alcotest.check_raises "empth path" Path.Empty_path (fun () ->
          Path.of_string ~env:`Unix (module S) "" |> ignore
        )
    );
]

let fun_tests = [
  "get identity", `Quick, (fun () ->
      Alcotest.(check string) "string" "foo" (Fun.ident "foo");
      Alcotest.(check int) "string" 1 (Fun.ident 1)
    );
  "flip argument order", `Quick, (fun () ->
      Alcotest.(check int) "flipping" (-3) (Fun.flip (-) 5 2)
    );
  "call teardown always", `Quick, (fun () ->
      let v = ref 0 in
      Fun.bracket ~setup:(fun () -> v := succ !v) (fun () -> ()) ~teardown:(fun () -> v := succ !v) |> ignore;
      Alcotest.(check int) "call teardown" 2 !v
    );
  "call teardown when raise exception", `Quick, (fun () ->
      let v = ref 0 in
      try
        Fun.bracket ~setup:(fun () -> v := succ !v) (fun () -> raise Not_found) ~teardown:(fun () -> v := succ !v) |> ignore;
      with Not_found -> ();
      Alcotest.(check int) "call teardown" 2 !v
    )
]

let testcases = [
  "Option", option_tests;
  "Fun", fun_tests;
  "Path", path_tests;
]

let () =
  Alcotest.run "Core functionally" testcases
