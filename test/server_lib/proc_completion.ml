open Sxfiler_core

module S = Sxfiler_server
module C = Sxfiler_server_core
module G = Sxfiler_server_gateway
module Co = Sxfiler_completion
module T = Sxfiler_server_translator

let proc_completion = [
  Alcotest_lwt.test_case "can setup common source" `Quick (fun switch () ->
      let module State = C.Statable.Make(struct
          type t = T.Completion.Item.t list
          let empty () = []
        end) in

      let module Setup_gateway : G.Completion.Setup = struct
        type params = {
          source: T.Completion.Item.t list;
        } [@@deriving yojson]

        type result = unit
        let handle {source} = State.update source

      end in

      let module Setup = S.Proc_completion.Setup(Setup_gateway) in
      let expected = [
        {T.Completion.Item.id = "1"; value = "foo"};
        {T.Completion.Item.id = "2"; value = "foobar"};
        {T.Completion.Item.id = "3"; value = "bar ball"};
      ] in
      let%lwt res = Setup.handle {Setup_gateway.source = expected} in
      let%lwt state = State.get () in
      Alcotest.(check @@ list @@ of_pp @@ Fmt.nop) "created" expected state;
      Alcotest.(check bool) "param" true (match Setup.params_of_json with
          | `Required _ -> true
          | _ -> false);
      Lwt.return_unit
    );
]

let testcases = [
  "rpc procedure : completion", proc_completion;
]
