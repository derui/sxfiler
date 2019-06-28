open Sxfiler_core

let option_tests =
  [ ( "detect none"
    , `Quick
    , fun () ->
      Alcotest.(check bool) "none is true" true (Option.is_none None) ;
      Alcotest.(check bool) "some is false" false (Option.is_none (Some 2)) )
  ; ( "detect some"
    , `Quick
    , fun () ->
      Alcotest.(check bool) "none is true" true (Option.is_some (Some 1)) ;
      Alcotest.(check bool) "none is false" false (Option.is_some None) )
  ; ( "get value from option with some"
    , `Quick
    , fun () -> Alcotest.(check int) "some" 1 (Option.get_exn (Some 1)) )
  ; ( "raise exception when None"
    , `Quick
    , fun () -> Alcotest.check_raises "none" Option.Not_some (fun () -> Option.get_exn None) )
  ; ( "get value from option with some"
    , `Quick
    , fun () -> Alcotest.(check int) "some" 1 @@ Option.get ~default:(fun () -> 100) (Some 1) )
  ; ( "get default value"
    , `Quick
    , fun () -> Alcotest.(check int) "none" 100 @@ Option.get ~default:(fun () -> 100) None )
  ; ( "get Some with some"
    , `Quick
    , fun () -> Alcotest.(check @@ option int) "option" (Some 10) @@ Option.some 10 )
  ; ( "allow to use option as monad"
    , `Quick
    , fun () ->
      let open Option.Infix in
      Alcotest.(check @@ option int) "option" (Some 10 >>= fun v -> Some (succ v))
      @@ Option.some 11 )
  ; ( "allow to handle none with monadic operator"
    , `Quick
    , fun () ->
      let open Option.Infix in
      Alcotest.(check @@ option int) "option" (None >>= fun v -> Some (succ v)) None )
  ; ( "allow to apply fmap to option"
    , `Quick
    , fun () ->
      let open Option.Infix in
      Alcotest.(check @@ option string) "option" (Some "bar" >|= fun v -> v ^ "foo")
      @@ Option.some "barfoo" ) ]

let result_tests =
  [ ( "allow to use result as monad"
    , `Quick
    , fun () ->
      let open Result.Infix in
      Alcotest.(check @@ result int string) "result" (Ok 10 >>= fun v -> Ok (succ v)) @@ Ok 11 )
  ; ( "allow to handle none with monadic operator"
    , `Quick
    , fun () ->
      let open Result.Infix in
      Alcotest.(check @@ result int string) "result" (Error "foo" >>= fun v -> Ok (succ v))
      @@ Error "foo" )
  ; ( "allow to apply fmap to result"
    , `Quick
    , fun () ->
      let open Result.Infix in
      Alcotest.(check @@ result string string) "result" (Ok "bar" >|= fun v -> v ^ "foo")
      @@ Ok "barfoo" )
  ; ( "allow to convert result to option"
    , `Quick
    , fun () ->
      Alcotest.(check @@ option string) "result" (Result.to_option (Ok "bar")) (Some "bar") ;
      Alcotest.(check @@ option string) "result" (Result.to_option (Error "bar")) None ) ]

let path_tests =
  let path_test = Alcotest.testable Path.pp Path.equal in
  [ ( "separator of path on unix"
    , `Quick
    , fun () ->
      let expected = "./a/b" in
      Alcotest.(check string) "unix" expected
      @@ Path.(to_string ~env:`Unix @@ of_string ~env:`Unix "./a/b") )
  ; ( "separator of path on windows"
    , `Quick
    , fun () ->
      let expected = ".\\a\\b" in
      Alcotest.(check string) "win" expected
      @@ Path.(to_string ~env:`Win @@ of_string ~env:`Win ".\\a\\b") )
  ; ( "ignore cwd if path is absolute"
    , `Quick
    , fun () ->
      let expected = "/bar/a" in
      Alcotest.(check string) "absolute" expected
      @@ Path.(to_string ~env:`Unix @@ of_string ~env:`Unix "/bar/a") )
  ; ( "allow to use windows device name"
    , `Quick
    , fun () ->
      let module S = struct
        let getcwd () = "c:\\foo"
      end in
      let expected = "c:\\foo\\a" in
      Alcotest.(check string) "absolute" expected
      @@ Path.(to_string ~env:`Win @@ resolve (module S) @@ of_string ~env:`Win "a") )
  ; ( "allow to contain . and .."
    , `Quick
    , fun () ->
      let module S = struct
        let getcwd () = "/var"
      end in
      let expected = "/bar" in
      Alcotest.(check string) "absolute" expected
      @@ Path.(
          to_string ~env:`Unix @@ resolve (module S) @@ of_string ~env:`Unix ".././foo/../bar")
    )
  ; ( "allow to create from list of component"
    , `Quick
    , fun () ->
      let module S = struct
        let getcwd () = "/var"
      end in
      let expected = "/bar" in
      Alcotest.(check string) "absolute" expected
      @@ Path.(
          to_string ~env:`Unix
          @@ resolve ~env:`Unix (module S)
          @@ of_list ~env:`Unix [".."; "."; "foo"; ".."; "bar"]) )
  ; ( "raise Empty_path from list"
    , `Quick
    , fun () -> Alcotest.check_raises "empty" Path.Empty_path (fun () -> ignore @@ Path.of_list [])
    )
  ; ( "resolve current and parent directory to realpath"
    , `Quick
    , fun () ->
      let module S = struct
        let getcwd () = "/var"
      end in
      let expected = "/bar" in
      Alcotest.(check string) "realpath" expected
      @@ Path.(
          to_string ~env:`Unix @@ resolve (module S) @@ of_string ~env:`Unix ".././foo/../bar")
    )
  ; ( "raise Empth_path"
    , `Quick
    , fun () ->
      Alcotest.check_raises "empth path" Path.Empty_path (fun () ->
          Path.of_string ~env:`Unix "" |> ignore) )
  ; ( "gets base name of a path"
    , `Quick
    , fun () ->
      let to_path = Path.of_string ~env:`Unix in
      Alcotest.(check string) "basename" "foo" @@ Path.basename @@ to_path "foo" ;
      Alcotest.(check string) "basename" "" @@ Path.basename @@ to_path "/" ;
      Alcotest.(check string) "basename" "bar" @@ Path.basename @@ to_path "foo/bar" )
  ; ( "gets dirname of a path"
    , `Quick
    , fun () ->
      let to_path = Path.of_string ~env:`Unix in
      Alcotest.(check string) "current" "." @@ Path.dirname @@ to_path "foo" ;
      Alcotest.(check string) "parent" ".." @@ Path.dirname @@ to_path "../bar" ;
      Alcotest.(check string) "dir" "foo" @@ Path.dirname @@ to_path "foo/bar" )
  ; ( "gets dirname of a path as path object"
    , `Quick
    , fun () ->
      let module S = struct
        let getcwd () = "/var"
      end in
      let to_full_path v = Path.(resolve ~env:`Unix (module S) @@ of_string ~env:`Unix v) in
      let to_path v = Path.(to_string ~env:`Unix @@ of_string ~env:`Unix v) in
      ( Alcotest.(check string) "current" (to_path "/var")
        @@ Path.(to_string ~env:`Unix @@ dirname_as_path @@ to_full_path "foo") ) ;
      Alcotest.(check string) "parent of root" (to_path "/")
      @@ Path.(to_string ~env:`Unix @@ dirname_as_path @@ to_full_path "..") )
  ; ( "gets dirname from resolved path"
    , `Quick
    , fun () ->
      let module S = struct
        let getcwd () = "/var"
      end in
      let to_path v = Path.resolve ~env:`Unix (module S) @@ Path.of_string ~env:`Unix v in
      Alcotest.(check string) "current" "/var" @@ Path.dirname @@ to_path "foo" ;
      Alcotest.(check string) "parent" "/" @@ Path.dirname @@ to_path "../bar" ;
      Alcotest.(check string) "dir" "/var/foo" @@ Path.dirname @@ to_path "foo/bar" )
  ; ( "equal same location"
    , `Quick
    , fun () ->
      let p path = Path.of_string ~env:`Unix path in
      Alcotest.(check path_test) "same path" (p "foo") (p "./foo") )
  ; ( "not equal between resolved and unresolved path"
    , `Quick
    , fun () ->
      let p path = Path.of_string ~env:`Unix path in
      let module S = struct
        let getcwd () = "/var"
      end in
      let resolved p = Path.resolve ~env:`Unix (module S) p in
      Alcotest.(check @@ neg @@ path_test) "same path" (p "foo" |> resolved) (p "foo") ) ]

let fun_tests =
  [ ( "get identity"
    , `Quick
    , fun () ->
      Alcotest.(check string) "string" "foo" (Fun.ident "foo") ;
      Alcotest.(check int) "string" 1 (Fun.ident 1) )
  ; ( "flip argument order"
    , `Quick
    , fun () -> Alcotest.(check int) "flipping" (-3) (Fun.flip ( - ) 5 2) )
  ; ( "call teardown always"
    , `Quick
    , fun () ->
      let v = ref 0 in
      Fun.bracket
        ~setup:(fun () -> v := succ !v)
        (fun () -> ())
        ~teardown:(fun () -> v := succ !v)
      |> ignore ;
      Alcotest.(check int) "call teardown" 2 !v )
  ; ( "call teardown when raise exception"
    , `Quick
    , fun () ->
      let v = ref 0 in
      try
        Fun.bracket
          ~setup:(fun () -> v := succ !v)
          (fun () -> raise Not_found)
          ~teardown:(fun () -> v := succ !v)
        |> ignore
      with Not_found ->
        () ;
        Alcotest.(check int) "call teardown" 2 !v )
  ; ( "return first value always"
    , `Quick
    , fun () ->
      Alcotest.(check int) "const" 1 Fun.(const 1 2) ;
      Alcotest.(check string) "diff type" "foo" Fun.(const "foo" 100) )
  ; ( "compose functions"
    , `Quick
    , fun () ->
      Alcotest.(check int) "compose same type" 3 Fun.((succ %> succ) 1) ;
      Alcotest.(check @@ float 0.0) "compose diff type" 2.0 Fun.((succ %> float_of_int) 1) )
  ; ( "reverse compose functions"
    , `Quick
    , fun () ->
      let sub a b = a - b in
      Alcotest.(check int) "compose same type" 0 Fun.((succ %< flip sub 2) 1) ;
      Alcotest.(check @@ float 0.0) "compose diff type" 2.0 Fun.((float_of_int %< succ) 1) )
  ; ( "shortcut apply"
    , `Quick
    , fun () ->
      Alcotest.(check int) "same type" 3 Fun.(succ & succ 1) ;
      Alcotest.(check @@ float 0.0) "diff type" 2.0 Fun.(float_of_int & succ 1) ) ]

let error_tests =
  [ ( "make simple error"
    , `Quick
    , fun () -> Alcotest.(check string) "error" "sample" Error.(create "sample" |> to_string) )
  ; ( "tagging error"
    , `Quick
    , fun () ->
      let error = Error.create "sample" in
      Alcotest.(check string) "errro" "tag: sample" Error.(tag error "tag" |> to_string) )
  ; ( "nest tagging"
    , `Quick
    , fun () ->
      let error = Error.create "sample" in
      let error = Error.tag error "tag1" in
      Alcotest.(check string) "errro" "tag2: tag1: sample" Error.(tag error "tag2" |> to_string)
    )
  ; ( "convert exception"
    , `Quick
    , fun () ->
      let error = Error.create "sample" in
      Alcotest.check_raises "exception" (Error.Error error) (fun () ->
          raise @@ Error.to_exn error) ) ]

let testcases =
  [ ("Option", option_tests)
  ; ("Fun", fun_tests)
  ; ("Path", path_tests)
  ; ("Result", result_tests)
  ; ("Error", error_tests) ]

let () = Alcotest.run "Core functionally" testcases
