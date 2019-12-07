module D = Sxfiler_domain

module G = struct
  type id = Uuidm.t

  let state = Random.get_state ()
  let generate = Uuidm.v4_gen state
end

let task_runner =
  [
    Alcotest_lwt.test_case "run asynchronous loops" `Quick (fun _ () ->
        let module T = Sxfiler_server_task in
        let module Tasker = (val T.Runner.make (module G) : T.Runner.Instance) in
        let stopper = Tasker.Runner.start Tasker.instance in
        Tasker.(Runner.stop instance);
        let%lwt () = stopper in
        Alcotest.(check @@ of_pp @@ Fmt.nop) "thread stopped" (Lwt.Return ()) (Lwt.state stopper);
        Lwt.return_unit);
    Alcotest_lwt.test_case "stop task that is running" `Quick (fun _ () ->
        let module T = Sxfiler_server_task in
        let module Tasker = (val T.Runner.make (module G) : T.Runner.Instance) in
        let stopper = Tasker.Runner.start Tasker.instance in
        let waiter, wakener = Lwt.task () in
        let task_id = G.generate () in
        let task =
          D.Task.make ~id:task_id
            ~executor:
              ( module struct
                let apply_interaction = `No_interaction
                let cancel () = ()

                let execute _ =
                  let count = ref 0 in
                  let rec loop () =
                    let open Lwt in
                    if !count > 3 then Lwt.wakeup wakener () else ();
                    incr count;
                    Lwt_unix.sleep 0.5 >>= loop
                  in
                  loop ()
              end )
        in
        let _ =
          Tasker.(
            Runner.subscribe instance ~f:(fun t ->
                if D.Task_types.equal_id t.id task_id then Lwt.return Tasker.(Runner.stop instance)
                else Lwt.return_unit))
        in
        let%lwt () = Tasker.(Runner.add_task instance ~task) in
        let%lwt () = waiter in
        let%lwt () = Tasker.(Runner.stop_task instance ~task:task_id) in
        let%lwt () = stopper in

        Alcotest.(check @@ of_pp @@ Fmt.nop) "thread stopped" (Lwt.Return ()) (Lwt.state stopper);
        Lwt.return_unit);
    Alcotest_lwt.test_case "allow to run task immediately" `Quick (fun _ () ->
        let module T = Sxfiler_server_task in
        let module Tasker = (val T.Runner.make (module G) : T.Runner.Instance) in
        let stopper = Tasker.Runner.start Tasker.instance in
        let data = ref 0 in
        let task =
          D.Task.make ~id:(G.generate ())
            ~executor:
              ( module struct
                let apply_interaction = `No_interaction
                let cancel () = ()

                let execute _ =
                  incr data;
                  Tasker.(Runner.stop instance);
                  Lwt.return_unit
              end )
        in
        let%lwt () = Tasker.(Runner.add_task instance ~task) in
        let%lwt () = stopper in
        Alcotest.(check @@ of_pp @@ Fmt.nop) "thread stopped" (Lwt.Return ()) (Lwt.state stopper);
        Alcotest.(check int) "task run" 1 !data;
        Lwt.return_unit);
    Alcotest_lwt.test_case "call subscriber when task finished" `Quick (fun _ () ->
        let module T = Sxfiler_server_task in
        let module Tasker = (val T.Runner.make (module G) : T.Runner.Instance) in
        let stopper = Tasker.Runner.start Tasker.instance in
        let task =
          D.Task.make ~id:(G.generate ())
            ~executor:
              ( module struct
                let apply_interaction = `No_interaction
                let cancel () = ()
                let execute _ = Lwt.return_unit
              end )
        in
        let _ =
          Tasker.(
            Runner.subscribe instance ~f:(fun t ->
                if t.id = task.id then Lwt.return Tasker.(Runner.stop instance) else Lwt.return_unit))
        in
        let%lwt () = Tasker.(Runner.add_task instance ~task) in
        let%lwt () = stopper in
        Alcotest.(check @@ of_pp @@ Fmt.nop) "thread stopped" (Lwt.Return ()) (Lwt.state stopper);
        Lwt.return_unit);
  ]

let () = Alcotest.run "Task library" [ ("task_runner", task_runner) ]
