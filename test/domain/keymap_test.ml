open Sxfiler_domain
module F = Test_fixtures.Testable
module K = Sxfiler_kbd

let test_set =
  [
    Alcotest_lwt.test_case_sync "return empty key map" `Quick (fun () ->
        let open Keymap in
        let items = bindings empty |> List.map fst in
        Alcotest.(check @@ list F.keymap_binding) "items" [] items);
    Alcotest_lwt.test_case_sync "add binding" `Quick (fun () ->
        let open Keymap in
        let context = Context.of_list [ "test" ] in
        let binding = Binding.make ~context ~key:K.(make "k") in
        let action = Action.make "foo" in
        let keymap = add ~binding ~action empty in
        let actual = bindings keymap in

        Alcotest.(check @@ list @@ pair F.keymap_binding F.keymap_action) "add" [ (binding, action) ] actual);
    Alcotest_lwt.test_case_sync "remove binding" `Quick (fun () ->
        let open Keymap in
        let context = Context.of_list [ "test" ] in
        let binding = Binding.make ~context ~key:K.(make "k") in
        let action = Action.make "foo" in
        let keymap = add ~binding ~action empty |> remove ~binding in

        Alcotest.(check F.keymap) "empty" empty keymap);
    Alcotest_lwt.test_case_sync "should be add same key with not same context" `Quick (fun () ->
        let open Keymap in
        let key = K.(make "k") in
        let context = Context.of_list [ "test" ] and context' = Context.of_list [ "another" ] in
        let binding = Binding.make ~context ~key in
        let binding' = Binding.make ~context:context' ~key in
        let action = Action.make "foo" and action' = Action.make "action2" in
        let keymap = add ~binding ~action empty |> add ~binding:binding' ~action:action' in
        let sort_by_binding = List.sort (fun (v1, _) (v2, _) -> Binding.compare v1 v2) in
        let actual = bindings keymap |> sort_by_binding in
        Alcotest.(check @@ list @@ pair F.keymap_binding F.keymap_action)
          "same key"
          ([ (binding, action); (binding', action') ] |> sort_by_binding)
          actual);
    Alcotest_lwt.test_case_sync "find all bindings by the key" `Quick (fun () ->
        let open Keymap in
        let key = K.(make "k") in
        let context = Context.of_list [ "test" ] and context' = Context.of_list [ "another" ] in
        let binding = Binding.make ~context ~key in
        let binding' = Binding.make ~context:context' ~key in
        let action = Action.make "foo" and action' = Action.make "action2" in
        let keymap = add ~binding ~action empty |> add ~binding:binding' ~action:action' in
        let sort_by_binding = List.sort Binding.compare in
        let actual = find_by ~key keymap |> sort_by_binding in
        Alcotest.(check @@ list F.keymap_binding) "same key" ([ binding; binding' ] |> sort_by_binding) actual);
  ]
