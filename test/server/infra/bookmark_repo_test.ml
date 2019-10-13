open Sxfiler_core
module D = Sxfiler_domain
module S = Sxfiler_server_core
module I = Sxfiler_server_infra

let state = Random.get_state ()

let obj =
  D.Bookmark.make ~id:Uuidm.(v4_gen state ()) ~path:Path.(of_string ~env:`Unix "/foo/bar") ~order:1

let obj2 =
  D.Bookmark.make ~id:Uuidm.(v4_gen state ()) ~path:Path.(of_string ~env:`Unix "/foo/baz") ~order:2

let test_set =
  [ Alcotest_lwt.test_case "should store a bookmark" `Quick (fun _ () ->
        let module State = S.Statable.Make (struct
          type t = D.Bookmark.t list

          let empty () = []
        end) in
        let module R = I.Bookmark_repo.Make (State) in
        let%lwt () = R.store obj in
        let%lwt obj' = R.resolve obj.id in
        Alcotest.(check bool) "stored" true D.Bookmark.(equal obj Option.(get_exn obj')) ;
        Lwt.return_unit)
  ; Alcotest_lwt.test_case "should store some bookmarks" `Quick (fun _ () ->
        let module State = S.Statable.Make (struct
          type t = D.Bookmark.t list

          let empty () = []
        end) in
        let module R = I.Bookmark_repo.Make (State) in
        let%lwt () = R.store obj in
        let%lwt () = R.store obj2 in
        let%lwt actual = R.find_all () in
        let actual = List.sort (fun v1 v2 -> compare v1.D.Bookmark.order v2.order) actual in
        Alcotest.(check @@ list @@ of_pp D.Bookmark.pp) "stored" [obj; obj2] actual ;
        Lwt.return_unit) ]
