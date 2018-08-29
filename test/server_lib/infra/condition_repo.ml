open Sxfiler_core
module D = Sxfiler_domain
module S = Sxfiler_server_core
module I = Sxfiler_server_infra

let testcases =
  [ Alcotest_lwt.test_case "can get current condition" `Quick (fun switch () ->
        let module State = S.Statable.Make (struct
            type t = D.Condition.t

            let empty () = D.Condition.empty
          end) in
        let module R = I.Condition_repo.Make (State) in
        let%lwt cond = R.resolve () in
        Alcotest.(check bool) "stored" true D.Condition.(equal empty cond) ;
        Lwt.return_unit )
  ; Alcotest_lwt.test_case "can enable/disable context" `Quick (fun switch () ->
        let module State = S.Statable.Make (struct
            type t = D.Condition.t

            let empty () = D.Condition.empty
          end) in
        let module R = I.Condition_repo.Make (State) in
        let expected = D.Condition.(empty |> enable ~context:"context") in
        let%lwt () = R.store expected in
        let%lwt actual = R.resolve () in
        Alcotest.(check bool) "stored" true (D.Condition.equal actual expected) ;
        Lwt.return_unit ) ]

let suite = [("condition repository", testcases)]
