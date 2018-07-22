open Sxfiler_core

module T = Sxfiler_types
module S = Sxfiler_server
module R = Sxfiler_rpc
module C = Sxfiler_server_core
module A = Sxfiler_server_action
module Jy = Jsonrpc_ocaml_yojson
module Rpcy = Sxfiler_rpc_yojson

let proc_configuration = [
  Alcotest_lwt.test_case "get current configuration" `Quick (fun switch () ->
      let expected = T.Configuration.{default with
                          viewer = Viewer.{default with current_stack_name = "foo"}
                         } in
      let module State = C.Statable.Make(struct
          type t = T.Configuration.t
          let empty () = expected
        end) in
      let module Get_sync = S.Proc_configuration.Get_sync(State) in

      let req = Jy.Request.{
          _method = "foo";
          params = None;
          id = Some Int64.zero;
        } in
      let%lwt res = Get_sync.handler req in
      let actual = match res.Jy.Response.result with
        | None -> Error ""
        | Some res -> Sxfiler_types_yojson.Configuration.of_yojson res
      in
      Alcotest.(check @@ result (of_pp @@ Fmt.nop) (of_pp @@ Fmt.nop)) "current" (Ok expected) actual;
      Lwt.return_unit
    );
]

let testcases = [
  "rpc procedure : confguration", proc_configuration;
]
