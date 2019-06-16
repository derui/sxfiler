module D = Sxfiler_domain
module S = Sxfiler_server_core
module I = Sxfiler_server_infra

let data =
  List.fold_left
    (fun keymap (key, value) -> D.Key_map.add keymap ~contexts:[] ~key ~value)
    (D.Key_map.make ())
    [(Sxfiler_kbd.make "k", "foo"); (Sxfiler_kbd.make "j", "bar")]

let test_set =
  [ Alcotest_lwt.test_case "can store keymap to state" `Quick (fun _ () ->
        let module State = S.Statable.Make (struct
            type t = D.Key_map.t

            let empty () = D.Key_map.make ()
          end) in
        let module R = I.Key_map_repo.Make (State) in
        let%lwt () = R.store data in
        let%lwt actual = State.get () in
        Alcotest.(check @@ of_pp Fmt.nop) "stored" data actual ;
        Lwt.return_unit )
  ; Alcotest_lwt.test_case "can get keymap stored" `Quick (fun _ () ->
        let module State = S.Statable.Make (struct
            type t = D.Key_map.t

            let empty () = data
          end) in
        let module R = I.Key_map_repo.Make (State) in
        let%lwt actual = R.resolve () in
        Alcotest.(check @@ of_pp Fmt.nop) "stored" data actual ;
        Lwt.return_unit ) ]
