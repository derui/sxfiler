module C = Sxfiler_core
module D = Sxfiler_domain
module U = Sxfiler_usecase
module G = Sxfiler_server_gateway
module Tr = Sxfiler_server_translator

let gen_id =
  let state = Random.State.make [||] in
  Uuidm.v4_gen state

let data =
  [ D.Bookmark.make ~id:(gen_id ()) ~path:C.Path.(of_string "/foo") ~order:1
  ; D.Bookmark.make ~id:(gen_id ()) ~path:C.Path.(of_string "/bar") ~order:2 ]

let test_set =
  [ Alcotest_lwt.test_case "get all bookmarks" `Quick (fun _ () ->
        let module BR = (val Test_fixtures.Memory_repository.bookmark_repository data) in
        let module Usecase = U.Bookmark.List_all.Make (BR) in
        let module Gateway = G.Bookmark.List_all.Make (Usecase) in
        let%lwt res = Gateway.handle () in
        let expected = List.map Tr.Bookmark.of_domain data in
        Alcotest.(check @@ result (list @@ of_pp Tr.Bookmark.pp) (of_pp Fmt.nop))
          "current" (Ok expected) res ;
        Lwt.return_unit)
  ; Alcotest_lwt.test_case "throw error when register same path" `Quick (fun _ () ->
        let module Usecase : U.Bookmark.Register.S = struct
          include U.Bookmark.Register.Type

          let execute _ = Lwt.return_error `Conflict
        end in
        let module Gateway = G.Bookmark.Register.Make (Usecase) in
        let%lwt res = Gateway.handle {path = "/foo"} in
        Alcotest.(check @@ result (of_pp Tr.Bookmark.pp) (of_pp Fmt.nop))
          "current" (Error G.Gateway_error.Bookmark_conflict) res ;
        Lwt.return_unit) ]
