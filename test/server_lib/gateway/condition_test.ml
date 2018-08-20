open Sxfiler_core

module D = Sxfiler_domain
module S = Sxfiler_server
module C = Sxfiler_server_core
module G = Sxfiler_server_gateway
module T = Sxfiler_server_translator
module U = Sxfiler_usecase

let condition_tests = [
  Alcotest_lwt.test_case "enable a context" `Quick (fun switch () ->
      let actual = ref D.Condition.empty in
      let expected = D.Condition.empty |> D.Condition.enable ~context:"foo" in
      let module Usecase = struct
        include U.Condition.Type

        let execute param =
          actual := D.Condition.empty |> D.Condition.enable ~context:param.context;
          Lwt.return_ok ()
        end in
      let module Gateway = G.Condition.Enable(Usecase) in

      let%lwt res = Gateway.handle {context = "foo"} in
      Alcotest.(check @@ of_pp Fmt.nop) "current" true D.Condition.(equal !actual expected);
      Lwt.return_unit
    );
  Alcotest_lwt.test_case "disable a context" `Quick (fun switch () ->
      let actual = ref (D.Condition.empty |> D.Condition.enable ~context:"foo") in
      let expected = D.Condition.empty in
      let module Usecase = struct
        include U.Condition.Type

        let execute param =
          actual := D.Condition.empty |> D.Condition.disable ~context:param.context;
          Lwt.return_ok ()
        end in
      let module Gateway = G.Condition.Disable(Usecase) in

      let%lwt res = Gateway.handle {context = "foo"} in
      Alcotest.(check @@ of_pp Fmt.nop) "current" true D.Condition.(equal !actual expected);
      Lwt.return_unit
    );
]

let testcases = [
  "rpc procedure : condition", condition_tests;
]
