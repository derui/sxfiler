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
      Alcotest.(check int) "some" 1 @@ Option.get ~default:100 (Some 1)
    );
  "get default value", `Quick, (fun () ->
      Alcotest.(check int) "none" 100 @@ Option.get ~default:100 None
    );
  "get Some with some", `Quick, (fun () ->
      Alcotest.(check @@ option int) "option" (Some 10) @@ Option.some 10
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
]

let () =
  Alcotest.run "Core functionally" testcases
