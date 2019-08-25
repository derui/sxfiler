module D = Sxfiler_domain
module U = Sxfiler_usecase
module G = Sxfiler_server_gateway
module Tr = Sxfiler_server_translator

let test_set =
  [ Alcotest_lwt.test_case "get current keybindings" `Quick (fun _ () ->
        let expected =
          List.fold_left
            (fun keymap (key, value) -> D.Key_map.add keymap ~contexts:[] ~key ~value)
            (D.Key_map.make ())
            [(Sxfiler_kbd.make "k", "foo"); (Sxfiler_kbd.make "j", "bar")]
        in
        let module Usecase = struct
          type input = unit
          type output = D.Key_map.t
          type error = unit

          let execute () = Lwt.return_ok expected
        end in
        let module Gateway = G.Keymap.Get.Make (Usecase) in
        let%lwt res = Gateway.handle () in
        Alcotest.(check @@ result (of_pp Tr.Key_map.pp) (of_pp Fmt.nop))
          "current"
          (Ok (Tr.Key_map.of_domain expected))
          res ;
        Lwt.return_unit) ]
