open Sxfiler_core
module D = Sxfiler_domain
module S = Sxfiler_server_core
module I = Sxfiler_server_infra

let data = List.fold_left (fun keymap (key, value) ->
    D.Key_map.add keymap ~condition:(D.Condition.empty) ~key ~value
  )
    (D.Key_map.make ())
    [
      (Sxfiler_kbd.make "k", "foo");
      (Sxfiler_kbd.make "j", "bar");
    ]

let testcases = [
  Alcotest_lwt.test_case "can store keymap to state" `Quick (fun switch () ->
      let module State = S.Statable.Make(struct
          type t = string D.Key_map.t
          let empty () = D.Key_map.make ()
        end) in
      let module R = I.Key_map_repo.Make(State) in

      let%lwt () = R.store data in
      let%lwt actual = State.get () in
      Alcotest.(check @@ of_pp Fmt.nop) "stored" data actual;
      Lwt.return_unit
    );
  Alcotest_lwt.test_case "can get keymap stored" `Quick (fun switch () ->
      let module State = S.Statable.Make(struct
          type t = string D.Key_map.t
          let empty () = data
        end) in
      let module R = I.Key_map_repo.Make(State) in

      let%lwt actual = R.resolve () in
      Alcotest.(check @@ of_pp Fmt.nop) "stored" data actual;
      Lwt.return_unit
    );
]

let suite = [
  "keymap repository", testcases;
]
