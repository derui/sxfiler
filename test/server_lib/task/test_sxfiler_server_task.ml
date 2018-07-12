module C = Sxfiler_server_core

let task_runner = [
  Alcotest_lwt.test_case "run asynchronous loops" `Quick (fun switch () ->
      let module T = Sxfiler_server_task in
      let module State = C.Statable.Make(struct
          type t = C.Root_state.t
          let empty () = C.Root_state.empty
        end) in

      let module Tasker = (val T.Runner.make (): T.Runner.Instance) in
      let%lwt () = Tasker.Runner.add_task_handler Tasker.instance ~name:"foo" ~handler:(fun _ _ -> Lwt.return_unit) in
      let stopper = Tasker.Runner.start Tasker.instance (module State) in

      Tasker.(Runner.stop instance);
      let%lwt () = stopper in
      Alcotest.(check @@ of_pp @@ Fmt.nop) "thread stopped" (Lwt.Return ()) (Lwt.state stopper);
      Lwt.return_unit
    );

  Alcotest_lwt.test_case "allow to run task immediately" `Quick (fun switch () ->
      let module T = Sxfiler_server_task in
      let module State = C.Statable.Make(struct
          type t = C.Root_state.t
          let empty () = C.Root_state.empty
        end) in
      let module Tasker = (val T.Runner.make (): T.Runner.Instance) in
      let%lwt () = Tasker.Runner.add_task_handler Tasker.instance ~name:"foo" ~handler:(fun _ _ -> Lwt.return_unit) in
      let stopper = Tasker.Runner.start Tasker.instance (module State) in
      let data = ref 0 in

      let instance = T.Intf.make_instance () (module Sxfiler_server_action.Dummy) (module struct
          type params = unit
          let plan = `No_plan

          let apply state params action =
            incr data;
            Tasker.(Runner.stop instance);
            Lwt.return @@ `Failed "foo"
        end)
      in

      let%lwt () = Tasker.Runner.add_task Tasker.instance instance in
      let%lwt () = stopper in
      Alcotest.(check @@ of_pp @@ Fmt.nop) "thread stopped" (Lwt.Return ()) (Lwt.state stopper);
      Alcotest.(check int) "task run" 1 !data;
      Lwt.return_unit
    );

]

let () =
  Alcotest.run "Task library" [
    "task_runner", task_runner;
  ]
