open Sxfiler_core

module T = Sxfiler_types
module S = Sxfiler_server
module R = Sxfiler_rpc
module C = Sxfiler_server_core
module A = Sxfiler_server_action
module Jy = Jsonrpc_ocaml_yojson
module Rpc = Sxfiler_rpc
module Rpcy = Sxfiler_rpc_yojson
module Co = Sxfiler_server_completion

module Dummy_comp = (struct
  module Completer = struct
    type t = unit
    let read (type v) () ~input ~collection ~stringify =
      let module S = (val stringify : Co.Completer.Type with type t = v) in
      let collection = List.map (fun v -> (S.to_string v, v)) collection in
      let regexp = Re.Posix.compile_pat input in

      List.map (fun s -> (Re.exec_opt regexp @@ fst s, snd s)) collection
      |> List.filter (fun v -> Option.is_some @@ fst v)
      |> List.map (fun (group, v) -> (Option.get_exn group, v))
      |> List.map (fun (group, v) ->
          let start, length = Re.Group.offset group 0 in
          {T.Completion.Candidate.start; length; value = v}
        )
  end
  let instance = ()
end : Co.Completer.Instance)


let proc_completion = [
  Alcotest_lwt.test_case "can setup common source" `Quick (fun switch () ->
      let module State = C.Statable.Make(struct
          type t = T.Completion.Common_item.t list
          let empty () = []
        end) in

      let module Setup_sync = S.Proc_completion.Setup_sync(State) in
      let module Ry = Rpcy.Completion.Setup_sync in
      let module R = Rpc.Completion.Setup_sync in
      let expected = [
        {T.Completion.Common_item.id = "1"; value = "foo"};
        {T.Completion.Common_item.id = "2"; value = "foobar"};
        {T.Completion.Common_item.id = "3"; value = "bar ball"};
      ] in
      let req = Jy.Request.{
          _method = "foo";
          params = Some Ry.(params_to_yojson {
              source = expected
            });
          id = Some Int64.zero;
        } in
      let%lwt res = Setup_sync.handler req in
      let%lwt state = State.get () in
      Alcotest.(check @@ list @@ of_pp @@ Fmt.nop) "created" expected state;
      Lwt.return_unit
    );

  Alcotest_lwt.test_case "can complete from common source stored before" `Quick (fun switch () ->
      let module State = C.Statable.Make(struct
          type t = T.Completion.Common_item.t list
          let empty () = [
            {T.Completion.Common_item.id = "1"; value = "foo"};
            {T.Completion.Common_item.id = "2"; value = "foobar"};
            {T.Completion.Common_item.id = "3"; value = "bar ball"};
          ]
        end) in

      let module Comp_state = C.Statable.Make(struct
          type t = (module Co.Completer.Instance)
          let empty () = (module Dummy_comp : Co.Completer.Instance)
        end) in

      let expected = Ok [|
          {
            T.Completion.Candidate.start = 0;
            length = 3;
            value = {T.Completion.Common_item.id = "1"; value = "foo"}
          };
          {
            T.Completion.Candidate.start = 0;
            length = 6;
            value = {T.Completion.Common_item.id = "2"; value = "foobar"};
          }
        |] in

      let module Read_sync = S.Proc_completion.Read_sync(State)(Comp_state) in
      let module Ry = Rpcy.Completion.Read_sync in
      let module R = Rpc.Completion.Read_sync in
      let req = Jy.Request.{
          _method = "foo";
          params = Some Ry.(params_to_yojson {
              input = "foo.*"
            });
          id = Some Int64.zero;
        } in
      let%lwt res = Read_sync.handler req in
      let result = Ry.result_of_yojson @@ Option.get_exn res.Jy.Response.result in
      Alcotest.(check @@ result  (of_pp @@ Fmt.nop) (of_pp @@ Fmt.nop)) "created" expected result;
      Lwt.return_unit
    );
]

let testcases = [
  "rpc procedure : completion", proc_completion;
]
