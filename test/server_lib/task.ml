open Sxfiler_core
open Sxfiler_domain

module C = Sxfiler_server_core

let scanner = [
  Alcotest_lwt.test_case "get snapshot of directory" `Quick (fun _ () ->
      let module T = Sxfiler_server.Task in
      let module A = Sxfiler_server_action in
      let open Lwt in
      let state = C.Root_state.empty in
      let%lwt data = T.Scanner.Jump.apply state {location = "foo"; name = "test"} (module A.Dummy) in
      Alcotest.(check @@ of_pp @@ Fmt.nop) "get snapshot" (`Update_scanner ("test", "foo", [])) data;
      return_unit
    );
]

let testcases = [
  "task Scanner.Move", scanner;
]
