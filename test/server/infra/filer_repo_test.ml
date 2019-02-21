open Sxfiler_core
module D = Sxfiler_domain
module S = Sxfiler_server_core
module I = Sxfiler_server_infra

let data =
  let file_tree = D.File_tree.make ~location:(Path.of_string "/var") ~nodes:[] in
  Test_fixtures.Filer.fixture "foo" ~file_tree ~sort_order:D.Types.Sort_type.Date

let test_set =
  [ Alcotest_lwt.test_case "can store filer to state" `Quick (fun _ () ->
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
  ; Alcotest_lwt.test_case "can get filer stored" `Quick (fun _ () ->
        let module State = S.Statable.Make (struct
            type t = S.Root_state.t

            let empty () = S.Root_state.(add_filer ~filer:data empty)
          end) in
        let module R = I.Filer_repo.Make (State) in
        let%lwt actual = R.resolve "foo" in
        Alcotest.(check @@ option @@ of_pp Fmt.nop) "stored" (Some data) actual ;
        Lwt.return_unit ) ]
