open Sxfiler_core

module T = Sxfiler_domain
module S = Sxfiler_server
module R = Sxfiler_rpc
module C = Sxfiler_server_core
module A = Sxfiler_server_action
module Jy = Jsonrpc_ocaml_yojson
module Rpcy = Sxfiler_rpc_yojson

let proc_scanner = [
  Alcotest_lwt.test_case "create new scanner if it does not exists" `Quick (fun switch () ->
      let task_finished, task_finished_waken = Lwt.wait () in
      let module Task = Sxfiler_server_task in
      let module State = C.Statable.Make(struct
          type t = C.Root_state.t
          let empty () = C.Root_state.empty
        end) in
      let module Handler = S.Task_result_handler.Make(struct
          let unixtime () = Int64.zero
        end)(struct
          let notify _ _ = Lwt.wakeup task_finished_waken (); Lwt.return_unit
        end) in

      let module Tasker = (val Task.Runner.make (): Task.Runner.Instance) in
      let module Make_sync = S.Proc_scanner.Make_sync(A.Dummy)(State)(Tasker) in
      let%lwt () = Tasker.Runner.add_task_handler Tasker.instance ~name:"foo" ~handler:Handler.handle in
      let stopper = Tasker.Runner.start Tasker.instance (module State) in

      let req = Jy.Request.{
          _method = "foo";
          params = Some Rpcy.Scanner.Make_sync.(params_to_yojson {
              initial_location = "/initial";
              name = "foo"
            });
          id = Some Int64.zero;
        } in
      let%lwt res = Make_sync.handler req in
      let%lwt () = task_finished in

      Tasker.Runner.stop Tasker.instance;

      let%lwt () = stopper in
      let%lwt state = State.get () in
      let scanner = C.Root_state.find_scanner ~name:"foo" state in
      let expected = Option.some @@ T.Scanner.make
          ~name:"foo"
          ~location:"/initial"
          ~nodes:[]
          ~history:T.Location_history.(make ()) in
      Alcotest.(check @@ option @@ of_pp @@ Fmt.nop) "created" expected scanner;
      Alcotest.(check @@ option @@ of_pp @@ Fmt.nop) "created" None res.Jy.Response.result;
      Alcotest.(check @@ option @@ of_pp @@ Fmt.nop) "created" None res.Jy.Response.error;
      Lwt.return_unit
    );

  Alcotest_lwt.test_case "do not create workspace if it exists" `Quick (fun switch () ->
      let module Task = Sxfiler_server_task in
      let module State = C.Statable.Make(struct
          type t = C.Root_state.t
          let empty () = C.Root_state.empty
        end) in
      let module Make_sync = S.Proc_scanner.Make_sync(A.Dummy)(State) in
      let module Handler = S.Task_result_handler.Make(struct
          let unixtime () = Int64.zero
        end)(struct
          let notify _ _ = Lwt.return_unit
        end) in

      let module Tasker = (val Task.Runner.make (): Task.Runner.Instance) in
      let module Make_sync = S.Proc_scanner.Make_sync(A.Dummy)(State)(Tasker) in
      let%lwt () = Tasker.Runner.add_task_handler Tasker.instance ~name:"foo" ~handler:Handler.handle in

      let req = Jy.Request.{
          _method = "foo";
          params = Some Rpcy.Scanner.Make_sync.(params_to_yojson {
              initial_location = "/initial";
              name = "foo"
            });
          id = Some Int64.zero;
        } in
      let%lwt () = State.with_lock (fun state ->
          let scanner = T.Scanner.make
              ~name:"foo"
              ~location:"/initial"
              ~nodes:[]
              ~history:T.Location_history.(make ()) in

          let state = C.Root_state.add_scanner ~scanner state in
          State.update state
        ) in
      let expected = Jy.(Exception.Jsonrpc_error (R.Errors.Scanner.already_exists, None)) in
      Alcotest.check_raises "raised" expected (fun () ->
          Lwt.ignore_result @@ Make_sync.handler req
        );
      Lwt.return_unit
    );
]

let testcases = [
  "rpc procedure : scanner", proc_scanner;
]
