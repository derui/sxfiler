open Sxfiler_core

let option_tests =
  [
    ( "allow to use option as monad",
      `Quick,
      fun () ->
        let open Option.Infix in
        Alcotest.(check @@ option int) "option" (Some 10 >>= fun v -> Some (succ v)) @@ Option.some 11 );
    ( "allow to use option as monad with binding operator",
      `Quick,
      fun () ->
        let open Option.Infix in
        let v =
          let* v = Some 10 in
          Some (succ v)
        in
        Alcotest.(check @@ option int) "option" v @@ Option.some 11 );
    ( "allow to handle none with monadic operator",
      `Quick,
      fun () ->
        let open Option.Infix in
        Alcotest.(check @@ option int) "option" (None >>= fun v -> Some (succ v)) None );
    ( "allow to apply fmap to option",
      `Quick,
      fun () ->
        let open Option.Infix in
        Alcotest.(check @@ option string) "option" (Some "bar" >>| fun v -> v ^ "foo") @@ Option.some "barfoo" );
  ]

let result_tests =
  [
    ( "allow to use result as monad",
      `Quick,
      fun () ->
        let open Result.Infix in
        Alcotest.(check @@ result int string) "result" (Ok 10 >>= fun v -> Ok (succ v)) @@ Ok 11 );
    ( "allow to use result as monad with binding operator",
      `Quick,
      fun () ->
        let open Result.Infix in
        let v =
          let* v = Ok 10 in
          Ok (succ v)
        in
        Alcotest.(check @@ result int string) "result" v @@ Ok 11 );
    ( "allow to handle none with monadic operator",
      `Quick,
      fun () ->
        let open Result.Infix in
        Alcotest.(check @@ result int string) "result" (Error "foo" >>= fun v -> Ok (succ v)) @@ Error "foo" );
    ( "allow to apply fmap to result",
      `Quick,
      fun () ->
        let open Result.Infix in
        Alcotest.(check @@ result string string) "result" (Ok "bar" >>| fun v -> v ^ "foo") @@ Ok "barfoo" );
  ]

let path_tests =
  let path_test = Alcotest.testable Path.pp Path.equal in
  [
    ( "separator of path on unix",
      `Quick,
      fun () ->
        let expected = Ok "./a/b" in
        Alcotest.(check @@ result string @@ of_pp Fmt.nop) "unix" expected
        @@ Path.(of_string ~env:`Unix "./a/b" |> Result.map @@ to_string ~env:`Unix) );
    ( "separator of path on windows",
      `Quick,
      fun () ->
        let expected = Ok ".\\a\\b" in
        Alcotest.(check @@ result string @@ of_pp Fmt.nop) "win" expected
        @@ Path.(of_string ~env:`Win ".\\a\\b" |> Result.map @@ to_string ~env:`Win) );
    ( "ignore cwd if path is absolute on Unix",
      `Quick,
      fun () ->
        let expected = Ok "/bar/a" in
        Alcotest.(check @@ result string @@ of_pp Fmt.nop) "absolute" expected
        @@ Path.(of_string ~env:`Unix "/bar/a" |> Result.map @@ to_string ~env:`Unix) );
    ( "ignore cwd if path is absolute on Windows",
      `Quick,
      fun () ->
        let expected = Ok "C:\\bar\\a" in
        Alcotest.(check @@ result string @@ of_pp Fmt.nop) "absolute" expected
        @@ Path.(of_string ~env:`Win "C:\\bar\\a" |> Result.map @@ to_string ~env:`Win) );
    ( "allow to use windows device name",
      `Quick,
      fun () ->
        let module S = struct
          let getcwd () = "c:\\foo"
        end in
        let expected = Ok "c:\\foo\\a" in
        Alcotest.(check @@ result string @@ of_pp Fmt.nop) "absolute" expected
        @@ Path.(of_string ~env:`Win ".\\a" |> Result.map @@ resolve (module S) |> Result.map @@ to_string ~env:`Win) );
    ( "allow to contain . and ..",
      `Quick,
      fun () ->
        let module S = struct
          let getcwd () = "/var"
        end in
        let expected = Ok "/bar" in
        Alcotest.(check @@ result string @@ of_pp Fmt.nop) "absolute" expected
        @@ Path.(
             of_string ~env:`Unix ".././foo/../bar"
             |> Result.map @@ resolve (module S)
             |> Result.map @@ to_string ~env:`Unix) );
    ( "allow to create from list of component",
      `Quick,
      fun () ->
        let module S = struct
          let getcwd () = "/var"
        end in
        let expected = Ok "/bar" in
        Alcotest.(check @@ result string @@ of_pp Fmt.nop) "absolute" expected
        @@ Path.(
             of_list ~env:`Unix [ ".."; "."; "foo"; ".."; "bar" ]
             |> Result.map @@ resolve ~env:`Unix (module S)
             |> Result.map @@ to_string ~env:`Unix) );
    ( "raise Empty_path from list",
      `Quick,
      fun () -> Alcotest.(check @@ result path_test @@ of_pp Fmt.nop) "empty" (Error Path.Empty_path) (Path.of_list [])
    );
    ( "resolve current and parent directory to realpath",
      `Quick,
      fun () ->
        let module S = struct
          let getcwd () = "/var"
        end in
        let expected = Ok "/bar" in
        Alcotest.(check @@ result string @@ of_pp Fmt.nop) "realpath" expected
        @@ Path.(
             of_string ~env:`Unix ".././foo/../bar"
             |> Result.map @@ resolve (module S)
             |> Result.map @@ to_string ~env:`Unix) );
    ( "raise Empth_path",
      `Quick,
      fun () ->
        Alcotest.(check @@ result path_test @@ of_pp Fmt.nop)
          "empty path" (Error Path.Empty_path)
          (Path.of_string ~env:`Unix "") );
    ( "gets base name of a path",
      `Quick,
      fun () ->
        let to_path v = Path.of_string ~env:`Unix v |> Result.map Path.basename in
        Alcotest.(check @@ result string @@ of_pp Fmt.nop) "basename" (Ok "foo") @@ to_path "foo";
        Alcotest.(check @@ result string @@ of_pp Fmt.nop) "basename" (Ok "") @@ to_path "/";
        Alcotest.(check @@ result string @@ of_pp Fmt.nop) "basename" (Ok "bar") @@ to_path "foo/bar" );
    ( "gets dirname of a path",
      `Quick,
      fun () ->
        let to_path v = Path.of_string ~env:`Unix v |> Result.map Path.dirname in
        let test = Alcotest.(check @@ result string @@ of_pp Fmt.nop) in
        test "current" (Ok ".") @@ to_path "foo";
        test "parent" (Ok "..") @@ to_path "../bar";
        test "dir" (Ok "foo") @@ to_path "foo/bar" );
    ( "gets dirname of a path as path object",
      `Quick,
      fun () ->
        let module S = struct
          let getcwd () = "/var"
        end in
        let to_full_path v = Path.(of_string ~env:`Unix v |> Result.map @@ resolve ~env:`Unix (module S)) in
        let to_path v = Path.(of_string ~env:`Unix v |> Result.map @@ to_string ~env:`Unix) in
        let test = Alcotest.(check @@ result string @@ of_pp Fmt.nop) in
        test "current" (to_path "/var")
          Path.(to_full_path "foo" |> Result.map @@ dirname_as_path |> Result.map @@ to_string ~env:`Unix);
        test "parent of root" (to_path "/")
          Path.(to_full_path ".." |> Result.map dirname_as_path |> Result.map @@ to_string ~env:`Unix) );
    ( "gets dirname from resolved path",
      `Quick,
      fun () ->
        let module S = struct
          let getcwd () = "/var"
        end in
        let to_path v =
          Path.of_string ~env:`Unix v |> Result.map @@ Path.resolve ~env:`Unix (module S) |> Result.map Path.dirname
        in
        let test = Alcotest.(check @@ result string @@ of_pp Fmt.nop) in
        test "current" (Ok "/var") @@ to_path "foo";
        test "parent" (Ok "/") @@ to_path "../bar";
        test "dir" (Ok "/var/foo") @@ to_path "foo/bar" );
    ( "equal same location",
      `Quick,
      fun () ->
        let p path = Path.of_string ~env:`Unix path in
        Alcotest.(check @@ result path_test @@ of_pp Fmt.nop) "same path" (p "foo") (p "./foo") );
    ( "not equal between resolved and unresolved path",
      `Quick,
      fun () ->
        let p path = Path.of_string ~env:`Unix path in
        let module S = struct
          let getcwd () = "/var"
        end in
        let resolved p = Path.resolve ~env:`Unix (module S) p in
        Alcotest.(check @@ neg @@ result path_test @@ of_pp Fmt.nop)
          "same path"
          (p "foo" |> Result.map resolved)
          (p "foo") );
    ( "join a place to path",
      `Quick,
      fun () ->
        let p path = Path.of_string ~env:`Unix path |> Result.get_ok in
        let path = p "/var" in
        Alcotest.(check @@ path_test) "with full path" (p "/var/foo") (Path.join ~env:`Unix path "/foo");
        Alcotest.(check @@ path_test) "with current directory" (p "/var/./foo") (Path.join ~env:`Unix path "./foo");
        Alcotest.(check @@ path_test) "with parent directory" (p "/var/../foo") (Path.join ~env:`Unix path "../foo");
        Alcotest.(check path_test) "ignore empty path" (p "/var") (Path.join ~env:`Unix path "") );
  ]

let fun_tests =
  [
    ( "get identity",
      `Quick,
      fun () ->
        Alcotest.(check string) "string" "foo" (Fun.ident "foo");
        Alcotest.(check int) "string" 1 (Fun.ident 1) );
    ("flip argument order", `Quick, fun () -> Alcotest.(check int) "flipping" (-3) (Fun.flip ( - ) 5 2));
    ( "call teardown always",
      `Quick,
      fun () ->
        let v = ref 0 in
        Fun.bracket ~setup:(fun () -> v := succ !v) (fun () -> ()) ~teardown:(fun () -> v := succ !v) |> ignore;
        Alcotest.(check int) "call teardown" 2 !v );
    ( "call teardown when raise exception",
      `Quick,
      fun () ->
        let v = ref 0 in
        try
          Fun.bracket ~setup:(fun () -> v := succ !v) (fun () -> raise Not_found) ~teardown:(fun () -> v := succ !v)
          |> ignore
        with Not_found ->
          ();
          Alcotest.(check int) "call teardown" 2 !v );
    ( "return first value always",
      `Quick,
      fun () ->
        Alcotest.(check int) "const" 1 Fun.(const 1 2);
        Alcotest.(check string) "diff type" "foo" Fun.(const "foo" 100) );
    ( "pipe functions",
      `Quick,
      fun () ->
        let open Fun.Infix in
        Alcotest.(check int) "pipe same type" 3 ((succ %> succ) 1);
        Alcotest.(check int) "pipe same type" 3 Fun.((pipe succ succ) 1);
        Alcotest.(check @@ float 0.0) "pipe diff type" 2.0 ((succ %> float_of_int) 1);
        Alcotest.(check @@ float 0.0) "pipe diff type" 2.0 Fun.((pipe succ float_of_int) 1) );
    ( "reverse compose functions",
      `Quick,
      fun () ->
        let sub a b = a - b in
        let open Fun.Infix in
        Alcotest.(check int) "compose same type" 2 ((succ % sub 2) 1);
        Alcotest.(check int) "compose same type" 0 Fun.(compose succ (flip sub 2) 1);
        Alcotest.(check @@ float 0.0) "compose diff type" 2.0 ((float_of_int % succ) 1);
        Alcotest.(check @@ float 0.0) "compose diff type" 2.0 Fun.((compose float_of_int succ) 1) );
    ( "shortcut apply",
      `Quick,
      fun () ->
        Alcotest.(check int) "same type" 3 (succ & succ 1);
        Alcotest.(check @@ float 0.0) "diff type" 2.0 (float_of_int & succ 1) );
  ]

let error_tests =
  [
    ( "make simple error",
      `Quick,
      fun () -> Alcotest.(check string) "error" "sample" Error.(create "sample" |> to_string) );
    ( "tagging error",
      `Quick,
      fun () ->
        let error = Error.create "sample" in
        Alcotest.(check string) "errro" "tag: sample" Error.(tag error "tag" |> to_string) );
    ( "nest tagging",
      `Quick,
      fun () ->
        let error = Error.create "sample" in
        let error = Error.tag error "tag1" in
        Alcotest.(check string) "errro" "tag2: tag1: sample" Error.(tag error "tag2" |> to_string) );
    ( "convert exception",
      `Quick,
      fun () ->
        let error = Error.create "sample" in
        Alcotest.check_raises "exception" (Error.Error error) (fun () -> raise @@ Error.to_exn error) );
  ]

let time_tests =
  let time_t = Alcotest.testable Time.pp Time.equal in
  [
    ( "get time of epoch",
      `Quick,
      fun () ->
        let time = Time.of_float 0. |> Option.fmap ~f:Time.to_int64 in
        Alcotest.(check @@ option int64) "time" (Some 0L) time );
    ( "get time of epoch",
      `Quick,
      fun () ->
        let time = Time.of_float 1.05 |> Option.fmap ~f:Time.to_int64 in
        Alcotest.(check @@ option int64) "time" (Some 1_050_000L) time );
    ( "get seconds in float of epoch",
      `Quick,
      fun () ->
        let v = 1.123456 in
        let time = Time.of_float v |> Option.fmap ~f:Time.to_float in
        Alcotest.(check @@ option @@ float 0.0000001) "time" (Some v) time );
    ( "get rf3339 of epoch",
      `Quick,
      fun () ->
        let time = Time.of_float 0. |> Option.fmap ~f:Time.to_rfc3339 in
        Alcotest.(check @@ option string) "time" (Some "1970-01-01T00:00:00.000000Z") time );
    ( "get valid date for leap year by epoch time",
      `Quick,
      fun () ->
        let time = Time.of_float 1580002902.025 |> Option.fmap ~f:Time.to_rfc3339 in
        Alcotest.(check @@ option string) "time" (Some "2020-01-26T01:41:42.025000Z") time );
    ( "print maximum time to be able to handle this module",
      `Quick,
      fun () -> Alcotest.(check string) "maximum" "9999-12-31T23:59:59.999999Z" (Time.to_rfc3339 Time.max) );
    ( "get time from string formatted by RFC3339",
      `Quick,
      fun () ->
        let formatted = Time.of_rfc3339 "1970-01-01T00:00:00Z" |> Option.map Time.to_rfc3339 in
        let frac = Time.of_rfc3339 "1970-01-01T00:00:00.01Z" |> Option.map Time.to_rfc3339 in
        Alcotest.(check @@ option string) "normal" (Some "1970-01-01T00:00:00.000000Z") formatted;
        Alcotest.(check @@ option string) "with frac" (Some "1970-01-01T00:00:00.010000Z") frac );
    ( "raise error when invalid format is given",
      `Quick,
      fun () ->
        Alcotest.(check @@ option time_t) "invalid month" None (Time.of_rfc3339 "1970-1-01T00:00:00Z");
        Alcotest.(check @@ option time_t) "invalid year" None (Time.of_rfc3339 "70-01-01T00:00:00Z");
        Alcotest.(check @@ option time_t) "invalid date" None (Time.of_rfc3339 "1970-01-1T00:00:00Z");
        Alcotest.(check @@ option time_t) "invalid hour" None (Time.of_rfc3339 "1970-01-01T0:00:00Z");
        Alcotest.(check @@ option time_t) "invalid minute" None (Time.of_rfc3339 "1970-01-01T00:0:00Z");
        Alcotest.(check @@ option time_t) "invalid second" None (Time.of_rfc3339 "1970-01-01T00:00:0Z");
        Alcotest.(check @@ option time_t) "invalid frac" None (Time.of_rfc3339 "1970-01-01T00:00:00.Z");
        Alcotest.(check @@ option time_t) "invalid offset" None (Time.of_rfc3339 "1970-01-01T00:00:00.0") );
  ]

let string_tests =
  [
    ( "empty list when apply split for empty string",
      `Quick,
      fun () -> Alcotest.(check @@ list string) "empty" [] & String.split_by_len ~len:1 "" );
    ( "singleton list when apply split by length that is large than size of string ",
      `Quick,
      fun () -> Alcotest.(check @@ list string) "empty" [ "abc" ] & String.split_by_len ~len:5 "abc" );
    ( "split to each character when pass 0 or minus value to 'len'",
      `Quick,
      fun () ->
        Alcotest.(check @@ list string) "list" [ "a"; "b"; "c" ] & String.split_by_len ~len:0 "abc";
        Alcotest.(check @@ list string) "list" [ "a"; "b"; "c" ] & String.split_by_len ~len:(-1) "abc" );
    ( "do not return same size when function can not divide length of string by len",
      `Quick,
      fun () -> Alcotest.(check @@ list string) "list" [ "ab"; "cd"; "e" ] & String.split_by_len ~len:2 "abcde" );
  ]

let testcases =
  [
    ("Option", option_tests);
    ("Fun", fun_tests);
    ("Path", path_tests);
    ("Result", result_tests);
    ("Error", error_tests);
    ("Time", time_tests);
    ("Comparable", Comparable_test.tests);
    ("String", string_tests);
  ]

let () = Alcotest.run "Core functionally" testcases
