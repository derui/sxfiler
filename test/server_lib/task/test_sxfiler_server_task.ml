
let task_runner = [
  Alcotest_lwt.test_case "run asynchronous loops" `Quick (fun switch () ->
      let module T = Sxfiler_server_task in

      let module Tasker = (val T.Runner.make (): T.Runner.Instance) in
      let stopper = Tasker.Runner.start Tasker.instance in

      Tasker.(Runner.stop instance);
      let%lwt () = stopper in
      Alcotest.(check @@ of_pp @@ Fmt.nop) "thread stopped" (Lwt.Return ()) (Lwt.state stopper);
      Lwt.return_unit
    );

  Alcotest_lwt.test_case "allow to run task immediately" `Quick (fun switch () ->
      let module T = Sxfiler_server_task in
      let module Tasker = (val T.Runner.make (): T.Runner.Instance) in
      let stopper = Tasker.Runner.start Tasker.instance  in
      let data = ref 0 in

      let instance = (module struct
                       module Task = struct
                         type t = unit
                         let run _ =
                           incr data;
                           Tasker.(Runner.stop instance);
                           Lwt.return_unit
                       end
                       let this = ()
                     end: T.Task.Instance)
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
