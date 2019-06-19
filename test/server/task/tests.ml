module D = Sxfiler_domain

module G = struct
  type id = Uuidm.t

  let state = Random.get_state ()
  let generate = Uuidm.v4_gen state
end

let task_runner =
  [ Alcotest_lwt.test_case "run asynchronous loops" `Quick (fun _ () ->
        let module T = Sxfiler_server_task in
        let module Tasker = (val T.Runner.make (module G) : T.Runner.Instance) in
        let stopper = Tasker.Runner.start Tasker.instance in
        Tasker.(Runner.stop instance) ;
        let%lwt () = stopper in
        Alcotest.(check @@ of_pp @@ Fmt.nop) "thread stopped" (Lwt.Return ()) (Lwt.state stopper) ;
        Lwt.return_unit )
  ; Alcotest_lwt.test_case "allow to run task immediately" `Quick (fun _ () ->
        let module T = Sxfiler_server_task in
        let module Tasker = (val T.Runner.make (module G) : T.Runner.Instance) in
        let stopper = Tasker.Runner.start Tasker.instance in
        let data = ref 0 in
        let task =
          D.Task.make ~id:(G.generate ())
            ~executor:
              ( module struct
                let apply_interaction = `No_interaction

                let execute _ =
                  incr data ;
                  Tasker.(Runner.stop instance) ;
                  Lwt.return_unit
              end )
        in
        let%lwt () = Tasker.(Runner.add_task instance ~task) in
        let%lwt () = stopper in
        Alcotest.(check @@ of_pp @@ Fmt.nop) "thread stopped" (Lwt.Return ()) (Lwt.state stopper) ;
        Alcotest.(check int) "task run" 1 !data ;
        Lwt.return_unit ) ]

let () = Alcotest.run "Task library" [("task_runner", task_runner)]
