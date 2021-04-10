open Sxfiler_core
module I = Sxfiler_infrastructure
module D = Sxfiler_domain
module F = Test_fixtures
module FL = Sxfiler_workflow

let keymap =
  let module K = Sxfiler_kbd in
  let bindings =
    [
      ([], K.of_keyseq "k" |> Option.get, "action1");
      ([ "foo" ], K.of_keyseq "j" |> Option.get, "action2");
      ([ "bar" ], K.of_keyseq "h" |> Option.get, "action3");
    ]
  in
  let keymap = D.Keymap.empty in
  List.fold_left
    (fun keymap (contexts, key, value) ->
      let binding = D.Keymap.Binding.make ~context:(D.Context.of_list contexts) ~key in
      let action = D.Keymap.Action.make value in
      D.Keymap.add ~binding ~action keymap)
    keymap bindings

let test_set =
  [
    Alcotest_lwt.test_case "resolve key map " `Quick (fun _ () ->
        let module S = I.Statable.Make (struct
          type t = D.Keymap.t

          let empty () = keymap
        end) in
        let module Instance = I.Keymap_step.Instance (S) in
        let%lwt resolved = Instance.resolve_keymap () in
        Alcotest.(check @@ F.Testable.keymap) "keymap" resolved keymap;
        Lwt.return_unit);
    Alcotest_lwt.test_case "store key map " `Quick (fun _ () ->
        let module S = I.Statable.Make (struct
          type t = D.Keymap.t

          let empty () = keymap
        end) in
        let binding = D.Keymap.Binding.make ~context:D.Context.empty ~key:(Sxfiler_kbd.of_keyseq "v" |> Option.get) in
        let action = D.Keymap.Action.make "action" in
        let keymap' = D.Keymap.add ~binding ~action keymap in
        let module Instance = I.Keymap_step.Instance (S) in
        let%lwt () = Instance.store_keymap keymap' in
        let%lwt stored = S.get () in
        Alcotest.(check @@ F.Testable.keymap) "keymap" stored keymap';
        Lwt.return_unit);
    Alcotest_lwt.test_case "load key map" `Quick (fun _ () ->
        let module S = I.Statable.Make (struct
          type t = D.Keymap.t

          let empty () = keymap
        end) in
        let make_keymap list =
          List.fold_left
            (fun keymap (key, contexts, action) ->
              let binding =
                D.Keymap.Binding.make ~context:(D.Context.of_list contexts)
                  ~key:(Sxfiler_kbd.of_keyseq key |> Option.get)
              in
              let action = D.Keymap.Action.make action in
              D.Keymap.add ~binding ~action keymap)
            D.Keymap.empty list
        in
        let keymap = make_keymap [ ("k", [], "action1"); ("j", [ "foo" ], "action2"); ("h", [ "bar" ], "action3") ] in
        let path = Path.of_list [ "."; "data_real"; "key_map"; "test.json" ] |> Result.get_ok in
        let module Instance = I.Keymap_step.Instance (S) in
        let%lwt keymap' = Instance.load_keymap path in
        Alcotest.(check & result F.Testable.keymap & of_pp Fmt.nop) "keymap" (Ok keymap) keymap';
        Lwt.return_unit);
    Alcotest_lwt.test_case "load invalid key map" `Quick (fun _ () ->
        let module S = I.Statable.Make (struct
          type t = D.Keymap.t

          let empty () = keymap
        end) in
        let path = Path.of_list [ "."; "data_real"; "key_map"; "invalid.json" ] |> Result.get_ok in
        let module Instance = I.Keymap_step.Instance (S) in
        let%lwt keymap' = Instance.load_keymap path in
        let error = Alcotest.testable FL.Common_step.Keymap.pp_load_error FL.Common_step.Keymap.equal_load_error in
        Alcotest.(check & result F.Testable.keymap error)
          "keymap"
          (Error (`Illegal_keymap "Keymap_file.error(Format_error(Line 1, bytes 36-39:\nInvalid token ']}\n'))"))
          keymap';
        Lwt.return_unit);
  ]
