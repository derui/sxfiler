open Sxfiler_core
open Sxfiler_types

module C = Sxfiler_server_core
module S = Sxfiler_server
module Rpcy = Sxfiler_rpc_yojson

let result_handler = [
  Alcotest_lwt.test_case "update workspace when result is Update_workspace" `Quick (fun _ () ->
      let notified = ref 0 in
      let module H = S.Task_result_handler.Make(struct
          let unixtime () = Int64.zero
        end)
          (struct
            let notify (type v) (type r)
                (module S:Jsonrpc_ocaml_yojson.Client.Api_def with type params = v
                                                               and type result = r)
                (param:v option) =
              incr notified;
              Lwt.return_unit
          end)
      in
      let module State = C.Statable.Make(struct
          type t = C.Root_state.t
          let empty () = C.Root_state.empty
        end)
      in
      let open Lwt in
      let current = Tree_snapshot.make ~directory:"foo" ~nodes:[] in
      let%lwt () = H.handle (module State) (`Update_workspace ("test", current)) in
      Alcotest.(check int) "notified" 1 !notified;
      return_unit
    );

  Alcotest_lwt.test_case "notify for updating workspace if Update_workspace" `Quick (fun _ () ->
      let notified = ref None in
      let module H = S.Task_result_handler.Make(struct
          let unixtime () = Int64.zero
        end)
          (struct
            let notify (type v) (type r)
                (module S:Jsonrpc_ocaml_yojson.Client.Api_def with type params = v
                                                               and type result = r)
                (param:v option) =

              notified := S.params_to_json param;
              Lwt.return_unit
          end)
      in
      let module State = C.Statable.Make(struct
          type t = C.Root_state.t
          let empty () = C.Root_state.empty
        end)
      in
      let open Lwt in
      let current = Tree_snapshot.make ~directory:"foo" ~nodes:[] in
      let%lwt () = H.handle (module State) (`Update_workspace ("test", current)) in
      let%lwt state =  State.get () in
      let module Ty = Sxfiler_types_yojson in
      let expected =
        match C.Root_state.find_workspace ~name:"test" state with
        | None -> None
        | Some workspace ->
          Some Rpcy.Notification.Workspace_update.(params_to_yojson {
              name = "test";
              workspace;
            }) in
      Alcotest.(check @@ option @@ of_pp @@ Fmt.nop) "notified" expected !notified;
      return_unit
    );

  Alcotest_lwt.test_case "update workspace if it already exists" `Quick (fun _ () ->
      let notified = ref None in
      let module Clock = struct
        let unixtime () = Int64.zero
      end in
      let module H = S.Task_result_handler.Make(Clock)
          (struct
            let notify (type v) (type r)
                (module S:Jsonrpc_ocaml_yojson.Client.Api_def with type params = v
                                                               and type result = r)
                (param:v option) =

              notified := S.params_to_json param;
              Lwt.return_unit
          end)
      in
      let module State = C.Statable.Make(struct
          type t = C.Root_state.t
          let empty () = C.Root_state.empty
        end)
      in
      let open Lwt in
      let current = Tree_snapshot.make ~directory:"foo" ~nodes:[] in
      let%lwt state = State.get () in
      let ws = Workspace.(make ~current ~history:Snapshot_history.(make ())) in
      let state = C.Root_state.add_workspace ~name:"test" ~ws state in
      let%lwt () = State.update state in
      let%lwt () = H.handle (module State) (`Update_workspace ("test", current)) in
      let%lwt state = State.get () in
      let module Ty = Sxfiler_types_yojson in
      let expected_ws = Workspace.replace_current ~snapshot:current ~clock:(module Clock) ws in
      let expected =
        match C.Root_state.find_workspace ~name:"test" state with
        | None -> None
        | Some workspace ->
          Some Rpcy.Notification.Workspace_update.(params_to_yojson {
              name = "test";
              workspace = expected_ws;
            }) in
      Alcotest.(check @@ option @@ of_pp @@ Fmt.nop) "notified" expected !notified;
      return_unit
    );
]

let testcases = [
  "task result handler", result_handler;
]
