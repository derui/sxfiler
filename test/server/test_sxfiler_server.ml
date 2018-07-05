open Sxfiler_core

module C = Sxfiler_server_core

let task_runner = [
  Alcotest_lwt.test_case "run asynchronous loops" `Quick (fun switch () ->
      let module T = Sxfiler_server_task in
      let get_state () = Lwt.return @@ C.Root_state.empty in
      let waken, stopper = T.Runner.start get_state in

      let open Lwt in
      Lwt.wakeup waken ();
      stopper >>= fun () ->
      Alcotest.(check @@ of_pp @@ Fmt.nop) "thread stopped" (Lwt.Return ()) (Lwt.state stopper);
      return_unit
    );
  Alcotest_lwt.test_case "allow to run task immediately" `Quick (fun switch () ->
      let module T = Sxfiler_server_task in
      let get_state () = Lwt.return @@ C.Root_state.empty in
      let waken, stopper = T.Runner.start get_state in
      let data = ref 0 in

      let instance = T.Intf.make_instance () (module Sxfiler_server_action.Dummy) (module struct
          type params = unit
          let plan = `No_plan

          let apply state params action =
            data := succ !data;
            Lwt.wakeup waken ();
            Lwt.return @@ `Failed "foo"
        end)
      in

      let open Lwt in
      T.Runner.add_task instance >>= fun () ->
      stopper >>= fun () ->
      Alcotest.(check @@ of_pp @@ Fmt.nop) "thread stopped" (Lwt.Return ()) (Lwt.state stopper);
      Alcotest.(check int) "task run" 1 !data;
      return_unit
    );
]

let () =
  Alcotest.run "server functionally" [
    "task runner", task_runner;
  ]
