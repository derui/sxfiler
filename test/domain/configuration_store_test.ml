module C = Sxfiler_domain.Configuration_store

let test_set =
  let json_t = Alcotest.testable Yojson.Basic.pp Yojson.Basic.equal in
  let key_t = Alcotest.testable C.Key.pp C.Key.equal in
  [
    Alcotest_lwt.test_case_sync "should empty store returns None always" `Quick (fun () ->
        let store = C.empty in
        let key = C.Key.from_list [ "a" ] |> Option.get in
        Alcotest.(check @@ option json_t) "get" None C.(get ~key store));
    Alcotest_lwt.test_case_sync "set key with value and get it after" `Quick (fun () ->
        let key = C.Key.from_list [ "a" ] |> Option.get in
        let store = C.empty |> C.put ~key ~value:(`String "foo") in
        Alcotest.(check @@ option json_t) "get" (Some (`String "foo")) C.(get ~key store));
    Alcotest_lwt.test_case_sync "list all keys in store" `Quick (fun () ->
        let key = C.Key.from_list [ "a"; "b" ] |> Option.get in
        let store = C.empty |> C.put ~key ~value:(`String "foo") in
        Alcotest.(check @@ list key_t) "get" [ key ] (C.keys store));
  ]
