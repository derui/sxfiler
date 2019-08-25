open Sxfiler_core
module D = Sxfiler_domain
module U = Sxfiler_usecase

let id_gen =
  let state = Random.State.make [||] in
  Uuidm.v4_gen state

let data = D.Bookmark.make ~id:(id_gen ()) ~path:Path.(of_list ["foo"; "bar"]) ~order:1

let make_mock ?id () =
  ( module struct
    type id = D.Bookmark.id

    let generate () = match id with None -> id_gen () | Some v -> v
  end : D.Id_generator_intf.Gen_random
    with type id = D.Bookmark.id )

let test_set =
  [ Alcotest_lwt.test_case "get all bookmark stored" `Quick (fun _ () ->
        let module KR = (val Test_fixtures.Memory_repository.bookmark_repository [data]) in
        let module Usecase = U.Bookmark.List_all.Make (KR) in
        let%lwt result = Usecase.execute () in
        Alcotest.(check @@ result (list @@ of_pp D.Bookmark.pp) (of_pp Fmt.nop))
          "bookmarks"
          (Ok [data])
          result ;
        Lwt.return_unit)
  ; Alcotest_lwt.test_case "should return created bookmark when register path" `Quick (fun _ () ->
        let module KR = (val Test_fixtures.Memory_repository.bookmark_repository []) in
        let module Usecase = U.Bookmark.Register.Make ((val make_mock ())) (KR) in
        let%lwt result = Usecase.execute {path = data.path} in
        let result =
          match result with
          | Ok v -> Ok (Path.equal v.path data.path && v.order = 1)
          | Error e -> Error e
        in
        Alcotest.(check @@ result bool (of_pp Fmt.nop)) "bookmark" (Ok true) result ;
        Lwt.return_unit)
  ; Alcotest_lwt.test_case "should store created bookmark to repository" `Quick (fun _ () ->
        let id = id_gen () in
        let module KR = (val Test_fixtures.Memory_repository.bookmark_repository []) in
        let module Usecase = U.Bookmark.Register.Make ((val make_mock ~id ())) (KR) in
        let%lwt _ = Usecase.execute {path = data.path} in
        let%lwt result = KR.resolve id in
        let data = D.Bookmark.make ~id ~path:data.path ~order:1 in
        Alcotest.(check @@ option @@ of_pp D.Bookmark.pp) "bookmark" (Some data) result ;
        Lwt.return_unit)
  ; Alcotest_lwt.test_case "should return conflict error when same path already registered" `Quick
        (fun _ () ->
           let data' = D.Bookmark.make ~id:(id_gen ()) ~path:data.path ~order:1 in
           let module KR = (val Test_fixtures.Memory_repository.bookmark_repository [data']) in
           let module Usecase = U.Bookmark.Register.Make ((val make_mock ())) (KR) in
           let%lwt result = Usecase.execute {path = data.path} in
           Alcotest.(check @@ result (of_pp D.Bookmark.pp) (of_pp Fmt.nop))
             "bookmark"
             (Error `Conflict)
             result ;
           Lwt.return_unit)
  ; Alcotest_lwt.test_case "should return the bookmark deleted" `Quick (fun _ () ->
        let data' = D.Bookmark.make ~id:(id_gen ()) ~path:data.path ~order:1 in
        let module KR = (val Test_fixtures.Memory_repository.bookmark_repository [data']) in
        let module Usecase = U.Bookmark.Delete.Make (KR) in
        let%lwt result = Usecase.execute {id = data'.id} in
        Alcotest.(check @@ result (of_pp D.Bookmark.pp) (of_pp Fmt.nop))
          "bookmark" (Ok data') result ;
        Lwt.return_unit)
  ; Alcotest_lwt.test_case "should delete the bookmark from repository" `Quick (fun _ () ->
        let data' = D.Bookmark.make ~id:(id_gen ()) ~path:data.path ~order:1 in
        let module KR = (val Test_fixtures.Memory_repository.bookmark_repository [data']) in
        let module Usecase = U.Bookmark.Delete.Make (KR) in
        let%lwt _ = Usecase.execute {id = data'.id} in
        let%lwt result = KR.resolve data'.id in
        Alcotest.(check @@ option @@ of_pp D.Bookmark.pp) "bookmark" None result ;
        Lwt.return_unit)
  ; Alcotest_lwt.test_case "should return error if not found id" `Quick (fun _ () ->
        let module KR = (val Test_fixtures.Memory_repository.bookmark_repository []) in
        let module Usecase = U.Bookmark.Delete.Make (KR) in
        let%lwt result = Usecase.execute {id = id_gen ()} in
        Alcotest.(check @@ result (of_pp D.Bookmark.pp) (of_pp Fmt.nop))
          "bookmark"
          (Error `Not_found)
          result ;
        Lwt.return_unit) ]
