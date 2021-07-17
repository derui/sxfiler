open Sxfiler_core
open Sxfiler_domain
module S = Sxfiler_dependency
module F = Test_fixtures.Testable
module K = Sxfiler_kbd
module FL = Sxfiler_workflow

let test_set =
  let module E = Common.Not_empty_string in
  let make_e = Option.get % E.make in
  let get_mock ?(store_keymap = fun _ -> Lwt.return_unit) ?(load_keymap = fun _ -> failwith "") resolve =
    (module struct
      let resolve_keymap () = Lwt.return resolve

      let store_keymap = store_keymap

      let load_keymap = load_keymap
    end : FL.Common_step.Keymap.Instance)
  in
  [
    Alcotest_lwt.test_case "add key binding by work flow" `Quick (fun _ () ->
        let open Keymap in
        let command =
          FL.Keymap.Add_key_binding.{ key = make_e "k"; context = [ make_e "test" ]; action = make_e "action" }
        in
        let mock = get_mock empty in
        let%lwt ret =
          FL.Keymap.(add_key_binding command)
          |> S.provide (function `Step_keymap_instance c -> S.Context.value mock c)
          |> S.run
        in
        let actual =
          empty
          |> add
               ~binding:
                 (Binding.make ~context:(Context.of_list [ "test" ]) ~key:(Sxfiler_kbd.of_keyseq "k" |> Option.get))
               ~action:(Action.make "action")
        in
        let event = Alcotest.testable FL.Keymap.pp_event FL.Keymap.equal_event in
        Alcotest.(check & result (list event) (of_pp FL.Keymap.pp_error)) "command" (Ok [ FL.Keymap.Added actual ]) ret;
        Lwt.return_unit);
    Alcotest_lwt.test_case "return error when invalid key sequence by add work flow" `Quick (fun _ () ->
        let open Keymap in
        let command =
          FL.Keymap.Add_key_binding.{ key = make_e "-a"; context = [ make_e "test" ]; action = make_e "action" }
        in
        let mock = get_mock empty in
        let%lwt ret =
          FL.Keymap.(add_key_binding command)
          |> S.provide (function `Step_keymap_instance c -> S.Context.value mock c)
          |> S.run
        in
        let actual = Error (FL.Keymap.Invalid_key "-a") in
        let event = Alcotest.testable FL.Keymap.pp_event FL.Keymap.equal_event in
        Alcotest.(check & result (list event) & of_pp FL.Keymap.pp_error) "command" actual ret;
        Lwt.return_unit);
    Alcotest_lwt.test_case "remove key binding by work flow" `Quick (fun _ () ->
        let open Keymap in
        let key = make_e "k" and context = [ make_e "test" ] in
        let command = FL.Keymap.Add_key_binding.{ key; context; action = make_e "action" } in
        let mock = get_mock empty in
        let%lwt add_result =
          FL.Keymap.(add_key_binding command)
          |> S.provide (function `Step_keymap_instance c -> S.Context.value mock c)
          |> S.run
        in
        let%lwt remove_result =
          match add_result with
          | Ok [ FL.Keymap.Added keymap ] ->
              FL.Keymap.(remove_key_binding { key; context })
              |> S.provide (function `Step_keymap_instance c -> S.Context.value (get_mock keymap) c)
              |> S.run
          | _                             -> failwith "Unknown path"
        in

        let actual = Ok [ FL.Keymap.Removed empty ] in
        let event = Alcotest.testable FL.Keymap.pp_event FL.Keymap.equal_event in
        Alcotest.(check & result (list event) & of_pp FL.Keymap.pp_error) "command" actual remove_result;
        Lwt.return_unit);
    Alcotest_lwt.test_case "return error when invalid key sequence by remove work flow" `Quick (fun _ () ->
        let open Keymap in
        let command = FL.Keymap.Remove_key_binding.{ key = make_e "-a"; context = [ make_e "test" ] } in
        let%lwt ret =
          FL.Keymap.(remove_key_binding command)
          |> S.provide (function `Step_keymap_instance c -> S.Context.value (get_mock empty) c)
          |> S.run
        in
        let actual = Error (FL.Keymap.Invalid_key "-a") in
        let event = Alcotest.testable FL.Keymap.pp_event FL.Keymap.equal_event in
        Alcotest.(check & result (list event) & of_pp FL.Keymap.pp_error) "command" actual ret;
        Lwt.return_unit);
    Alcotest_lwt.test_case "reload whole key map from path" `Quick (fun _ () ->
        let key = Sxfiler_kbd.of_keyseq "k" |> Option.get and context = [ "test" ] in
        let binding = Keymap.Binding.make ~context:(Context.of_list context) ~key in
        let keymap = Keymap.add ~binding ~action:(Keymap.Action.make "action") Keymap.empty in
        let load_keymap _ = Lwt.return_ok keymap in
        let store_keymap keymap' =
          Alcotest.(check F.keymap) "keymap" keymap keymap';
          Lwt.return_unit
        in

        FL.Keymap.(reload { path = Path.of_string "/" |> Result.get_ok })
        |> S.provide (function `Step_keymap_instance c ->
               S.Context.value (get_mock keymap ~load_keymap ~store_keymap) c)
        |> S.run |> Lwt.ignore_result;
        Lwt.return_unit);
    Alcotest_lwt.test_case "error when can not load key map from path" `Quick (fun _ () ->
        let load_keymap _ = Lwt.return_error `Not_found in
        let store_keymap _ =
          Alcotest.fail "invalid flow" |> ignore;
          Lwt.return_unit
        in

        let%lwt ret =
          FL.Keymap.(reload { path = Path.of_string "/" |> Result.get_ok })
          |> S.provide (function `Step_keymap_instance c ->
                 S.Context.value (get_mock Keymap.empty ~load_keymap ~store_keymap) c)
          |> S.run
        in
        let event = Alcotest.testable FL.Keymap.pp_event FL.Keymap.equal_event in
        Alcotest.(check & result (list event) & of_pp FL.Keymap.pp_error)
          "error"
          (Error (FL.Keymap.Invalid_keymap `Not_found))
          ret;
        Lwt.return_unit);
  ]
