open Sxfiler_core
module D = Sxfiler_domain
module R = Sxfiler_rpc
module G = Sxfiler_generated
module Pb = Ocaml_protoc_plugin
module F = Sxfiler_workflow

let test_set =
  let keymap = D.Keymap.empty in
  [
    Alcotest_lwt.test_case "call add_key_binding work flow with valid input" `Quick (fun _ () ->
        let expected =
          D.Keymap.add
            ~binding:(D.Keymap.Binding.make ~context:(D.Context.of_list [ "test" ]) ~key:(Sxfiler_kbd.make "k"))
            ~action:(D.Keymap.Action.make "expected") keymap
        in

        let work_flow _ = Lwt.return_ok [ F.Keymap.Added expected ] in
        let request =
          {
            G.Service.Request.id = "id";
            command = G.Service.Command.KEYMAP_ADD_KEY_BINDING;
            payload =
              { G.Keymap.AddKeyBindingRequest.key = "k"; contexts = [ "test" ]; action = "expected" }
              |> G.Keymap.AddKeyBindingRequest.to_proto |> Pb.Writer.contents |> Bytes.of_string;
          }
        in
        let%lwt res, events = R.Keymap_endpoint.add_key_binding work_flow request in
        Alcotest.(check & string) "same id" "id" res.id;
        Alcotest.(check & list & of_pp F.pp_event) "events" [ F.Keymap (F.Keymap.Added expected) ] events;
        Lwt.return_unit);
    Alcotest_lwt.test_case "get error when invalid key sequence" `Quick (fun _ () ->
        let work_flow _ = Lwt.return_error (F.Keymap.Invalid_key "K-S") in
        let request =
          {
            G.Service.Request.id = "id";
            command = G.Service.Command.KEYMAP_ADD_KEY_BINDING;
            payload =
              { G.Keymap.AddKeyBindingRequest.key = "K-S"; contexts = [ "test" ]; action = "expected" }
              |> G.Keymap.AddKeyBindingRequest.to_proto |> Pb.Writer.contents |> Bytes.of_string;
          }
        in
        let%lwt res, _ = R.Keymap_endpoint.add_key_binding work_flow request in
        Alcotest.(check & string) "same id" "id" res.id;
        Alcotest.(check & of_pp G.Service.Status.pp) "invalid" G.Service.Status.COMMAND_FAILED res.status;
        Alcotest.(check int) "error status" (-102) (res.error |> Option.get |> fun v -> v.status);
        Lwt.return_unit);
    Alcotest_lwt.test_case "get error when give empty values" `Quick (fun _ () ->
        let work_flow _ =
          Alcotest.fail "invalid path" |> ignore;
          Lwt.return_ok []
        in
        let request =
          {
            G.Service.Request.id = "id";
            command = G.Service.Command.KEYMAP_ADD_KEY_BINDING;
            payload =
              { G.Keymap.AddKeyBindingRequest.key = ""; contexts = [ "test" ]; action = "expected" }
              |> G.Keymap.AddKeyBindingRequest.to_proto |> Pb.Writer.contents |> Bytes.of_string;
          }
        in
        let%lwt res, _ = R.Keymap_endpoint.add_key_binding work_flow request in
        Alcotest.(check & string) "same id" "id" res.id;
        Alcotest.(check & of_pp G.Service.Status.pp) "invalid" G.Service.Status.COMMAND_FAILED res.status;
        Alcotest.(check int) "error status" (-102) (res.error |> Option.get |> fun v -> v.status);
        Lwt.return_unit);
    Alcotest_lwt.test_case "call reload flow in reload endpoint" `Quick (fun _ () ->
        let actual = Path.of_string ~env:`Unix "/foo.json" |> Result.get_ok in
        let work_flow { F.Keymap.Reload.path } =
          let path_test = Alcotest.testable Path.pp Path.equal in
          Alcotest.(check @@ path_test) "flow called" actual path;
          Lwt.return_ok []
        in
        let request =
          {
            G.Service.Request.id = "id";
            command = G.Service.Command.KEYMAP_RELOAD;
            payload = G.Keymap.ReloadRequest.to_proto () |> Pb.Writer.contents |> Bytes.of_string;
          }
        in
        let%lwt res, _ = R.Keymap_endpoint.reload actual work_flow request in
        Alcotest.(check & string) "same id" "id" res.id;
        Alcotest.(check & of_pp G.Service.Status.pp) "invalid" G.Service.Status.SUCCESS res.status;
        Lwt.return_unit);
  ]
