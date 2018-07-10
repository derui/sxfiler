open Sxfiler_core

module T = Sxfiler_types
module S = Sxfiler_server
module C = Sxfiler_server_core
module A = Sxfiler_server_action
module Jy = Jsonrpc_ocaml_yojson
module Rpcy = Sxfiler_rpc_yojson

let workspace_op = [
  Alcotest_lwt.test_case "create new workspace if it does not exists" `Quick (fun switch () ->
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
      let module Make_sync = S.Workspace_op.Make_sync(A.Dummy)(State)(Tasker) in
      let%lwt () = Tasker.Runner.add_task_handler Tasker.instance ~handler:Handler.handle in
      let stopper = Tasker.Runner.start Tasker.instance (module State) in

      let req = Jy.Request.{
          _method = "foo";
          params = Some Rpcy.Workspace.Make_sync.(params_to_yojson {
              initial_directory = "/initial";
              name = "foo"
            });
          id = Some Int64.zero;
        } in
      let%lwt res = Make_sync.handler req in
      let%lwt () = task_finished in

      Tasker.Runner.stop Tasker.instance;

      let%lwt () = stopper in
      let%lwt state = State.get () in
      let ws = C.Root_state.find_workspace ~name:"foo" state in
      let expected = Option.some @@ T.Workspace.make ~current:T.Tree_snapshot.(make ~directory:"/initial" ~nodes:[])
          ~history:T.Snapshot_history.(make ()) in
      let res_expected = Some Rpcy.Workspace.Make_sync.(result_to_yojson {created = false}) in
      Alcotest.(check @@ option @@ of_pp @@ Fmt.nop) "created" expected ws;
      Alcotest.(check @@ option @@ of_pp @@ Fmt.nop) "created" res_expected res.Jy.Response.result;
      Lwt.return_unit
    );

  Alcotest_lwt.test_case "do not create workspace if it exists" `Quick (fun switch () ->
      let module Task = Sxfiler_server_task in
      let module State = C.Statable.Make(struct
          type t = C.Root_state.t
          let empty () = C.Root_state.empty
        end) in
      let module Make_sync = S.Workspace_op.Make_sync(A.Dummy)(State) in
      let module Handler = S.Task_result_handler.Make(struct
          let unixtime () = Int64.zero
        end)(struct
          let notify _ _ = Lwt.return_unit
        end) in

      let module Tasker = (val Task.Runner.make (): Task.Runner.Instance) in
      let module Make_sync = S.Workspace_op.Make_sync(A.Dummy)(State)(Tasker) in
      let%lwt () = Tasker.Runner.add_task_handler Tasker.instance ~handler:Handler.handle in
      let stopper = Tasker.Runner.start Tasker.instance (module State) in

      let req = Jy.Request.{
          _method = "foo";
          params = Some Rpcy.Workspace.Make_sync.(params_to_yojson {
              initial_directory = "/initial";
              name = "foo"
            });
          id = Some Int64.zero;
        } in
      let%lwt () = State.with_lock (fun state ->
          let ws = T.Workspace.make ~current:T.Tree_snapshot.(make ~directory:"/initial" ~nodes:[])
              ~history:T.Snapshot_history.(make ()) in

          let state = C.Root_state.add_workspace ~name:"foo" ~ws state in
          State.update state
        ) in
      let%lwt res = Make_sync.handler req in
      Tasker.(Runner.stop instance);
      let%lwt () = stopper in
      let res_expected = Some Rpcy.Workspace.Make_sync.(result_to_yojson {created = true}) in
      Alcotest.(check @@ option @@ of_pp @@ Fmt.nop) "created" res_expected res.Jy.Response.result;
      Lwt.return_unit
    );
]

let testcases = [
  "workspace rpc operations", workspace_op;
]
