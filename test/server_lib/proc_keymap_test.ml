module D = Sxfiler_domain
module S = Sxfiler_server
module U = Sxfiler_usecase
module C = Sxfiler_server_core
module Jy = Jsonrpc_ocaml_yojson
module G = Sxfiler_server_gateway
module Tr = Sxfiler_server_translator
module T = Sxfiler_rpc.Types

let test_set =
  [ Alcotest_lwt.test_case "get current keybindings" `Quick (fun _ () ->
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
        let spec = S.Proc_keymap.get_spec (module Gateway) in
        let id = Random.int64 Int64.max_int in
        let%lwt res =
          spec.S.Procedure.handler Jy.Request.{_method = ""; params = None; id = Some id}
        in
        let module G = Sxfiler_server_gateway in
        Alcotest.(check @@ of_pp Fmt.nop)
          "current" res
          Jy.Response.
            { id = Some id
            ; error = None
            ; result = Some (Tr.Key_map.of_domain expected |> Tr.Key_map.to_yojson) } ;
        Lwt.return_unit ) ]
