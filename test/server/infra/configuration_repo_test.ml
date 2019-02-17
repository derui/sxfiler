module D = Sxfiler_domain
module S = Sxfiler_server_core
module I = Sxfiler_server_infra

let data = D.Configuration.{default_sort_order = D.Types.Sort_type.Date}

let test_set =
  [ Alcotest_lwt.test_case "can store configuration to state" `Quick (fun _ () ->
        let module State = S.Statable.Make (struct
            type t = S.Root_state.t

            let empty () = S.Root_state.empty
          end) in
        let module R = I.Configuration_repo.Make (State) in
        let%lwt () = R.store data in
        let%lwt actual = State.get () in
        Alcotest.(check @@ of_pp Fmt.nop) "stored" data actual.S.Root_state.configuration ;
        Lwt.return_unit )
  ; Alcotest_lwt.test_case "can get keymap stored" `Quick (fun _ () ->
        let module State = S.Statable.Make (struct
            type t = S.Root_state.t

            let empty () = {S.Root_state.empty with configuration = data}
          end) in
        let module R = I.Configuration_repo.Make (State) in
        let%lwt actual = R.resolve () in
        Alcotest.(check @@ of_pp Fmt.nop) "stored" data actual ;
        Lwt.return_unit ) ]
