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
        let keymap' = D.Key_map.add ~condition:D.Condition.empty ~key ~value:"foo" keymap in
        let module KR = (val Test_fixtures.Memory_repository.key_map_repository keymap) in
        let module Usecase = U.Keymap.Store.Make (KR) in
        let%lwt result = Usecase.execute keymap' in
        Alcotest.(check @@ result (of_pp Fmt.nop) (of_pp Fmt.nop)) "key map" (Ok ()) result ;
        Lwt.return_unit )
  ; Alcotest_lwt.test_case "add context and return subset of key map" `Quick (fun _ () ->
        let condition_k = D.Condition.of_list ["k"] and condition_j = D.Condition.of_list ["j"] in
        let key_k = Fun.(Sxfiler_kbd.of_keyseq %> Option.get_exn) "k"
        and key_j = Fun.(Sxfiler_kbd.of_keyseq %> Option.get_exn) "j" in
        let keymap =
          D.Key_map.add ~condition:condition_j ~key:key_j ~value:"bar" (D.Key_map.make ())
          |> D.Key_map.add ~condition:condition_k ~key:key_k ~value:"foo"
        in
        let module CR = struct
          let data = ref D.Condition.empty
          let resolve () = Lwt.return !data
          let store v = Lwt.return (data := v)
        end in
        let module KR = (val Test_fixtures.Memory_repository.key_map_repository keymap) in
        let module Usecase = U.Keymap.Add_context.Make (CR) (KR) in
        let%lwt result = Usecase.execute {context = "k"} in
        let expected = D.Key_map.subset ~condition:D.Condition.(of_list ["k"]) keymap in
        Alcotest.(check @@ result (of_pp Fmt.nop) (of_pp Fmt.nop)) "key map" (Ok expected) result ;
        Lwt.return_unit )
  ; Alcotest_lwt.test_case "delete context and return subset of key map" `Quick (fun _ () ->
        let condition_k = D.Condition.of_list ["k"] and condition_j = D.Condition.of_list ["j"] in
        let key_k = Fun.(Sxfiler_kbd.of_keyseq %> Option.get_exn) "k"
        and key_j = Fun.(Sxfiler_kbd.of_keyseq %> Option.get_exn) "j" in
        let keymap =
          D.Key_map.add ~condition:condition_j ~key:key_j ~value:"bar" (D.Key_map.make ())
          |> D.Key_map.add ~condition:condition_k ~key:key_k ~value:"foo"
        in
        let module CR = struct
          let data = ref D.Condition.(of_list ["k"; "j"])
          let resolve () = Lwt.return !data
          let store v = Lwt.return (data := v)
        end in
        let module KR = (val Test_fixtures.Memory_repository.key_map_repository keymap) in
        let module Usecase = U.Keymap.Delete_context.Make (CR) (KR) in
        let%lwt result = Usecase.execute {context = "j"} in
        let expected = D.Key_map.subset ~condition:D.Condition.(of_list ["k"]) keymap in
        Alcotest.(check @@ result (of_pp Fmt.nop) (of_pp Fmt.nop)) "key map" (Ok expected) result ;
        Lwt.return_unit ) ]
