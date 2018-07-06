open Sxfiler_core
open Sxfiler_types

module C = Sxfiler_server_core

let take_snapshot = [
  "do not make plan", `Quick, (fun () ->
      let module T = Sxfiler_server.Task in
      Alcotest.(check @@ of_pp @@ Fmt.nop) "no plan" `No_plan T.File.Take_snapshot.plan
    );
  Alcotest_lwt.test_case "get snapshot of directory" `Quick (fun _ () ->
      let module T = Sxfiler_server.Task in
      let module A = Sxfiler_server_action in
      let open Lwt in
      let state = C.Root_state.empty in
      let%lwt data = T.File.Take_snapshot.(apply state {directory = "foo"; workspace_name = "test"} (module A.Dummy)) in
      let snapshot = Tree_snapshot.make ~directory:"foo" ~nodes:[] in
      Alcotest.(check @@ of_pp @@ Fmt.nop) "get snapshot" (`Update_workspace ("test", snapshot)) data;
      return_unit
    );
]

let testcases = [
  "task File.Take_snapshot", take_snapshot;
]
