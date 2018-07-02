open Sxfiler_core

module C = Sxfiler_server_core

let task_runner = [
  Alcotest_lwt.test_case "run asynchronous loops" `Quick (fun switch () ->
      let module T = Sxfiler_server_task in
      let get_state () = Lwt.return @@ C.State.empty in
      let waken, stopper = T.Runner.start get_state in

      let open Lwt in
      Lwt.wakeup waken ();
      stopper >>= fun () ->
      Alcotest.(check @@ of_pp @@ Fmt.nop) "thread stopped" (Lwt.Return ()) (Lwt.state stopper);
      return_unit
    );
]

let () =
  Alcotest.run "server functionally" [
    "task runner", task_runner;
  ]
