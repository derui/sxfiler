open Sxfiler_core
module D = Sxfiler_domain
module U = Sxfiler_usecase

let test_set =
  [ Alcotest_lwt.test_case "get current key map" `Quick (fun _ () ->
        let keymap = D.Key_map.make () in
        let module CR = struct
          let store _ = assert false
          let resolve () = Lwt.return D.Condition.empty
        end in
        let module KR = (val Test_fixtures.Memory_repository.key_map_repository keymap) in
        let module Usecase = U.Keymap.Get.Make (CR) (KR) in
        let%lwt result = Usecase.execute () in
        Alcotest.(check @@ result (of_pp Fmt.nop) (of_pp Fmt.nop)) "key map" (Ok keymap) result ;
        Lwt.return_unit )
  ; Alcotest_lwt.test_case "store key map" `Quick (fun _ () ->
        let keymap = D.Key_map.make () in
        let key = Fun.(Sxfiler_kbd.of_keyseq %> Option.get_exn) "k" in
        let keymap' = D.Key_map.add ~contexts:["contexts"] ~key ~value:"foo" keymap in
        let module KR = (val Test_fixtures.Memory_repository.key_map_repository keymap) in
        let module Usecase = U.Keymap.Store.Make (KR) in
        let%lwt result = Usecase.execute keymap' in
        Alcotest.(check @@ result (of_pp Fmt.nop) (of_pp Fmt.nop)) "key map" (Ok ()) result ;
        Lwt.return_unit ) ]
