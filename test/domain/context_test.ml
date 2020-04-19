module C = Sxfiler_domain.Context

let test_set =
  [
    Alcotest_lwt.test_case_sync "should empty condition contains empty it" `Quick (fun () ->
        Alcotest.(check bool) "subset" true C.(subset empty ~parts:empty));
    Alcotest_lwt.test_case_sync "should return true to compare same condition" `Quick (fun () ->
        let expected = C.of_list [ "completing" ] in
        Alcotest.(check bool) "equal" true C.(equal expected expected));
    Alcotest_lwt.test_case_sync "should return true if current condition contains parts condition" `Quick (fun () ->
        let expected = C.of_list [ "completing" ] in
        let actual = C.empty in
        Alcotest.(check bool) "equal" true C.(subset expected ~parts:actual));
    Alcotest_lwt.test_case_sync "should return what expected condition is contains" `Quick (fun () ->
        let expected = C.of_list [ "completing" ] in
        let actual = C.of_list [ "completing"; "file_tree" ] in
        Alcotest.(check bool) "subset" true C.(subset actual ~parts:expected));
    Alcotest_lwt.test_case_sync "should return false expected condition is not subset of current" `Quick (fun () ->
        let expected = C.of_list [ "completing" ] in
        let actual = C.of_list [ "completing"; "file_tree" ] in
        Alcotest.(check bool) "subset" false C.(subset ~parts:actual expected));
  ]
