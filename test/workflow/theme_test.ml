open Sxfiler_core
open Sxfiler_domain
module F = Test_fixtures.Testable
module FL = Sxfiler_workflow
module S = Sxfiler_dependency

let test_set =
  let module E = Common.Not_empty_string in
  let make_e = Option.get % E.make in
  let color_code_t = Alcotest.testable Theme.Color_code.pp Theme.Color_code.equal
  and key_t = Alcotest.testable Common.Not_empty_string.pp Common.Not_empty_string.equal in
  let color_pair_t = Alcotest.pair key_t color_code_t in
  let get_mock ?(store_theme = fun _ _ -> failwith "") ?(get_current_theme = fun () -> failwith "") () =
    ( module struct
      let store_theme = store_theme

      let get_current_theme = get_current_theme
    end : FL.Common_step.Theme.Instance )
  in
  [
    Alcotest_lwt.test_case "add theme by work flow" `Quick (fun _ () ->
        let open Theme in
        let command =
          FL.Theme.Update_theme.
            {
              base_theme = None;
              color_codes = [ (make_e "color_a", make_e "#fff"); (make_e "color_b", make_e "#000") ];
            }
        in
        let expected =
          [
            (make_e "color_a", Color_code.of_string "#fff" |> Option.get);
            (make_e "color_b", Color_code.of_string "#000" |> Option.get);
          ]
        in
        let store_theme _ _ = Lwt.return_ok expected in
        let mock = get_mock ~store_theme () in
        let%lwt ret =
          FL.Theme.(update_theme command)
          |> S.provide (function `Step_theme_instance c -> S.Context.value mock c)
          |> S.run
        in
        Alcotest.(check & result (list color_pair_t) (of_pp FL.Theme.pp_error)) "command" (Ok expected) ret;
        Lwt.return_unit);
    Alcotest_lwt.test_case "return error from workflow to add theme if color code is invalid" `Quick (fun _ () ->
        let command =
          FL.Theme.Update_theme.
            { base_theme = None; color_codes = [ (make_e "color_a", make_e "#fff"); (make_e "color_b", make_e "#00") ] }
        in
        let store_theme _ _ = Lwt.return_ok [] in
        let mock = get_mock ~store_theme () in
        let%lwt ret =
          FL.Theme.(update_theme command)
          |> S.provide (function `Step_theme_instance c -> S.Context.value mock c)
          |> S.run
        in
        let expected = Error FL.Theme.(Invalid_color_format [ ("color_b", "#00") ]) in
        Alcotest.(check & result (list color_pair_t) (of_pp FL.Theme.pp_error)) "command" expected ret;
        Lwt.return_unit);
  ]
