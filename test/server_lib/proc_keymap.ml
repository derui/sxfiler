open Sxfiler_core

module T = Sxfiler_domain
module S = Sxfiler_server
module R = Sxfiler_rpc
module C = Sxfiler_server_core
module A = Sxfiler_server_action
module Jy = Jsonrpc_ocaml_yojson

let proc_keymap = [
  Alcotest_lwt.test_case "get current keybindings" `Quick (fun switch () ->
      let expected = List.fold_left (fun keymap (key, value) ->
          T.Key_map.add keymap ~condition:(T.Condition.empty) ~key ~value
        )
          (T.Key_map.make "empty")
          [
            (Sxfiler_kbd.make "k", "foo");
            (Sxfiler_kbd.make "j", "bar");
          ] in
      let module State = C.Statable.Make(struct
          type t = string T.Key_map.t
          let empty () = expected
        end) in
      let module Get_sync = S.Proc_keymap.Get_sync(State) in

      let req = Jy.Request.{
          _method = "foo";
          params = None;
          id = Some Int64.zero;
        } in
      let%lwt res = Get_sync.handler req in
      let module G = Sxfiler_server_gateway in
      let actual = res.Jy.Response.result in
      let expected = G.Keymap.Get_sync.result_to_yojson expected in
      Alcotest.(check @@ option @@ of_pp @@ Fmt.nop) "current" Option.(some expected) actual;
      Lwt.return_unit
    );
]

let testcases = [
  "rpc procedure : keymap", proc_keymap;
]
