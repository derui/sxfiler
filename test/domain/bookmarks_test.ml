open Sxfiler_domain
module C = Sxfiler_core
module F = Test_fixtures

let test_set =
  [
    Alcotest_lwt.test_case_sync "return empty list from empty" `Quick (fun () ->
        let open Bookmarks in
        let items = items empty in
        Alcotest.(check @@ list F.Testable.bookmark_item) "items" [] items);
    Alcotest_lwt.test_case_sync "add bookmark if it is not exists" `Quick (fun () ->
        let open Bookmarks in
        let path = C.Path.(of_string "/foo") |> Result.get_ok in
        let name = Name.make "name" in
        let bookmarks = empty |> insert ~name ~path in
        let items = items bookmarks |> List.map (fun Item.{ name; _ } -> name) in
        Alcotest.(check @@ list F.Testable.bookmark_name) "items" [ name ] items);
    Alcotest_lwt.test_case_sync "replace item if same name exists" `Quick (fun () ->
        let open Bookmarks in
        let path = C.Path.(of_string "/foo") |> Result.get_ok in
        let next_path = C.Path.(of_string "/bar") |> Result.get_ok in
        let name = Name.make "name" in
        let bookmarks = empty |> insert ~name ~path |> insert ~name ~path:next_path in
        let items = items bookmarks |> List.map (fun Item.{ path; _ } -> path) in
        Alcotest.(check @@ list @@ of_pp C.Path.pp) "items" [ next_path ] items);
    Alcotest_lwt.test_case_sync "remove item from bookmarks" `Quick (fun () ->
        let open Bookmarks in
        let path = C.Path.(of_string "/foo") |> Result.get_ok in
        let name = Name.make "name" in
        let bookmarks = empty |> insert ~name ~path |> remove name in
        let items = items bookmarks in
        Alcotest.(check @@ list F.Testable.bookmark_item) "items" [] items);
    Alcotest_lwt.test_case_sync "do not raise error when remove bookmark that is not exists in bookmarks" `Quick
      (fun () ->
        let open Bookmarks in
        let name = Name.make "name" in
        let bookmarks = empty |> remove name in
        let items = items bookmarks in
        Alcotest.(check @@ list F.Testable.bookmark_item) "items" [] items);
  ]
