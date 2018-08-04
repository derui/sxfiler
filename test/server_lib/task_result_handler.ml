open Sxfiler_core
open Sxfiler_domain

module C = Sxfiler_server_core
module S = Sxfiler_server
module Rpcy = Sxfiler_rpc_yojson

let result_handler = [
  Alcotest_lwt.test_case "update scanner when result is Update_scanner" `Quick (fun _ () ->
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
      let%lwt state = State.get () in
      let scanner = Scanner.make ~name:"foo" ~nodes:[] ~location:"not tested" ~history:(Location_history.make ()) in
      let%lwt () = State.update @@ C.Root_state.add_scanner ~scanner state in
      let%lwt () = H.handle (module State) (`Update_scanner ("foo", "test", [])) in
      Alcotest.(check int) "notified" 1 !notified;
      return_unit
    );

  Alcotest_lwt.test_case "notify for updating workspace if Update_workspace" `Quick (fun _ () ->
      let notified = ref None in
      let module Clock = struct
        let unixtime () = Int64.zero
      end in
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
      let%lwt state = State.get () in
      let scanner = Scanner.make ~name:"foo" ~nodes:[] ~location:"not tested" ~history:(Location_history.make ()) in
      let%lwt () = State.update @@ C.Root_state.add_scanner ~scanner state in
      let%lwt () = H.handle (module State) (`Update_scanner ("foo", "test", [])) in
      let module Ty = Sxfiler_domain_yojson in
      let expected =
        Some Rpcy.Notification.Scanner_update.(params_to_yojson {
            name = "foo";
            scanner = Scanner.move_location scanner ~location:"test" ~nodes:[] (module Clock);
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
      let%lwt state = State.get () in
      let scanner = Scanner.(make ~name:"test" ~nodes:[] ~location:"foo" ~history:Location_history.(make ())) in
      let state = C.Root_state.add_scanner ~scanner state in
      let%lwt () = State.update state in
      let%lwt () = H.handle (module State) (`Update_scanner ("test", "foobar", [])) in
      let%lwt state = State.get () in
      let module Ty = Sxfiler_domain_yojson in
      let expected =
        Some Rpcy.Notification.Scanner_update.(params_to_yojson {
            name = "test";
            scanner = Scanner.move_location ~location:"foobar" ~nodes:[] scanner (module Clock);
          }) in
      Alcotest.(check @@ option @@ of_pp @@ Fmt.nop) "notified" expected !notified;
      return_unit
    );
]

let testcases = [
  "task result handler", result_handler;
]
