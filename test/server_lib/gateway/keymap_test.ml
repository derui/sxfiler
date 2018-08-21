open Sxfiler_core

module D = Sxfiler_domain
module S = Sxfiler_server
module U = Sxfiler_usecase
module C = Sxfiler_server_core
module G = Sxfiler_server_gateway
module Tr = Sxfiler_server_translator

let keymap_tests = [
  Alcotest_lwt.test_case "get current keybindings" `Quick (fun switch () ->
      let expected = List.fold_left (fun keymap (key, value) ->
          D.Key_map.add keymap ~condition:(D.Condition.empty) ~key ~value
        )
          (D.Key_map.make ())
          [
            (Sxfiler_kbd.make "k", "foo");
            (Sxfiler_kbd.make "j", "bar");
          ] in
      let module Usecase = struct
        type input = unit
        type output = string D.Key_map.t

        let execute () = Lwt.return_ok expected
      end in
      let module Gateway = G.Keymap.Get(Usecase) in

      let%lwt res = Gateway.handle () in
      Alcotest.(check @@ of_pp Fmt.nop) "current"
        (Tr.Key_map.of_domain expected) res;
      Lwt.return_unit
    );
]

let testcases = [
  "rpc procedure : keymap", keymap_tests;
]