open Sxfiler_core
module D = Sxfiler_domain
module S = Sxfiler_server_core
module I = Sxfiler_server_infra

let data =
  D.Filer.make ~id:"foo" ~location:(Path.of_string "/var") ~nodes:[]
    ~sort_order:D.Types.Sort_type.Date ~history:(D.Location_history.make ())


let testcases =
  [ Alcotest_lwt.test_case "can store filer to state" `Quick (fun switch () ->
        let module State = S.Statable.Make (struct
            type t = S.Root_state.t

            let empty () = S.Root_state.empty
          end) in
        let module R = I.Filer_repo.Make (State) in
        let%lwt () = R.store data in
        let%lwt actual = State.get () in
        Alcotest.(check @@ option @@ of_pp Fmt.nop)
          "stored" (Some data)
          (S.Root_state.find_filer ~id:"foo" actual) ;
        Lwt.return_unit )
  ; Alcotest_lwt.test_case "can get keymap stored" `Quick (fun switch () ->
        let module State = S.Statable.Make (struct
            type t = S.Root_state.t

            let empty () = S.Root_state.(add_filer ~filer:data empty)
          end) in
        let module R = I.Filer_repo.Make (State) in
        let%lwt actual = R.resolve "foo" in
        Alcotest.(check @@ option @@ of_pp Fmt.nop) "stored" (Some data) actual ;
        Lwt.return_unit ) ]


let suite = [("filer repository", testcases)]
