open Sxfiler_core

module T = Sxfiler_domain
module S = Sxfiler_server
module R = Sxfiler_rpc
module C = Sxfiler_server_core
module A = Sxfiler_server_action
module Jy = Jsonrpc_ocaml_yojson
module Rpcy = Sxfiler_rpc_yojson

let proc_keybindings = [
  Alcotest_lwt.test_case "get current keybindings" `Quick (fun switch () ->
      let expected = `Assoc [
          ("foo", `Int 100);
          ("bar", `String "foobar");
        ] in
      let module State = C.Statable.Make(struct
          type t = Yojson.Safe.json
          let empty () = expected
        end) in
      let module Get_sync = S.Proc_keybindings.Get_sync(State) in

      let req = Jy.Request.{
          _method = "foo";
          params = None;
          id = Some Int64.zero;
        } in
      let%lwt res = Get_sync.handler req in
      let actual = res.Jy.Response.result in
      Alcotest.(check @@ option @@ of_pp @@ Fmt.nop) "current" Option.(some expected) actual;
      Lwt.return_unit
    );
]

let testcases = [
  "rpc procedure : keybindings", proc_keybindings;
]
