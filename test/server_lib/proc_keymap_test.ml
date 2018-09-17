open Sxfiler_core
module D = Sxfiler_domain
module S = Sxfiler_server
module U = Sxfiler_usecase
module C = Sxfiler_server_core
module Jy = Jsonrpc_ocaml_yojson
module G = Sxfiler_server_gateway
module Tr = Sxfiler_server_translator
module T = Sxfiler_rpc.Types

let proc_keymap =
  [ Alcotest_lwt.test_case "get current keybindings" `Quick (fun switch () ->
        let expected =
          List.fold_left
            (fun keymap (key, value) ->
               D.Key_map.add keymap ~condition:D.Condition.empty ~key ~value )
            (D.Key_map.make ())
            [(Sxfiler_kbd.make "k", "foo"); (Sxfiler_kbd.make "j", "bar")]
        in
        let module State = C.Statable.Make (struct
            type t = string D.Key_map.t

            let empty () = expected
          end) in
        let module Gateway = struct
          type params = unit
          type result = T.Key_map.t

          let handle () = Lwt.map Tr.Key_map.of_domain @@ State.get ()
        end in
        let module Get = S.Proc_keymap.Get (Gateway) in
        let%lwt res = Get.handle () in
        let module G = Sxfiler_server_gateway in
        Alcotest.(check @@ of_pp Fmt.nop) "current" (Tr.Key_map.of_domain expected) res ;
        Alcotest.(check bool)
          "params" true
          (match Get.params_of_json with `Not_required _ -> true | _ -> false) ;
        Lwt.return_unit ) ]

let () = Alcotest.run "Keymap procedures" [("get", proc_keymap)]
