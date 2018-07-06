open Sxfiler_core

module C = Sxfiler_server_core

let task_runner = [
  Alcotest_lwt.test_case "run asynchronous loops" `Quick (fun switch () ->
      let module T = Sxfiler_server_task in
      let module State = C.Statable.Make(struct
          type t = C.Root_state.t
          let empty () = C.Root_state.empty
        end) in
      let waken, stopper = T.Runner.start (module State) (fun _ _ -> Lwt.return_unit) in

      let open Lwt in
      Lwt.wakeup waken ();
      let%lwt () = stopper in
      Alcotest.(check @@ of_pp @@ Fmt.nop) "thread stopped" (Lwt.Return ()) (Lwt.state stopper);
      return_unit
    );
  Alcotest_lwt.test_case "allow to run task immediately" `Quick (fun switch () ->
      let module T = Sxfiler_server_task in
      let module State = C.Statable.Make(struct
          type t = C.Root_state.t
          let empty () = C.Root_state.empty
        end) in
      let waken, stopper = T.Runner.start (module State) (fun _ _ -> Lwt.return_unit) in
      let data = ref 0 in

      let instance = T.Intf.make_instance () (module Sxfiler_server_action.Dummy) (module struct
          type params = unit
          let plan = `No_plan

          let apply state params action =
            incr data;
            Lwt.wakeup waken ();
            Lwt.return @@ `Failed "foo"
        end)
      in

      let open Lwt in
      T.Runner.add_task instance >>= fun () ->
      let%lwt () = stopper in
      Alcotest.(check @@ of_pp @@ Fmt.nop) "thread stopped" (Lwt.Return ()) (Lwt.state stopper);
      Alcotest.(check int) "task run" 1 !data;
      return_unit
    );
]

let testcases = [
  "task runner", task_runner;
]

let () =
  let tests = testcases
              @ Task.testcases
              @ Rpc_connection.testcases
              @ Task_result_handler.testcases
  in
  Alcotest.run "server functionally" tests
