open Sxfiler_core
module D = Sxfiler_domain
module S = Sxfiler_server_core
module I = Sxfiler_server_infra

let data = D.Scanner.make ~id:"foo"
    ~location:(Path.of_string (module System.Real) "/var")
    ~nodes:[]
    ~history:(D.Location_history.make ())

let testcases = [
  Alcotest_lwt.test_case "can store scanner to state" `Quick (fun switch () ->
      let module State = S.Statable.Make(struct
          type t = S.Root_state.t
          let empty () = S.Root_state.empty
        end) in
      let module R = I.Scanner_repo.Make(State) in

      let%lwt () = R.store data in
      let%lwt actual = State.get () in
      Alcotest.(check @@ option @@ of_pp Fmt.nop) "stored" (Some data)
        (S.Root_state.find_scanner ~id:"foo" actual);
      Lwt.return_unit
    );
  Alcotest_lwt.test_case "can get keymap stored" `Quick (fun switch () ->
      let module State = S.Statable.Make(struct
          type t = S.Root_state.t
          let empty () = S.Root_state.(add_scanner ~scanner:data empty)
        end) in
      let module R = I.Scanner_repo.Make(State) in

      let%lwt actual = R.resolve "foo" in
      Alcotest.(check @@ option @@ of_pp Fmt.nop) "stored" (Some data) actual;
      Lwt.return_unit
    );
]

let suite = [
  "scanner repository", testcases;
]
