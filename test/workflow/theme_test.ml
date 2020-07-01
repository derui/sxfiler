open Sxfiler_core
open Sxfiler_domain
module F = Test_fixtures.Testable
module K = Sxfiler_kbd
module FL = Sxfiler_workflow

let test_set =
  let module E = Common.Not_empty_string in
  let make_e = Option.get % E.make in
  let theme_t = Alcotest.testable Theme.pp Theme.equal in
  [
    Alcotest_lwt.test_case "add theme by work flow" `Quick (fun _ () ->
        let open Theme in
        let command =
          FL.Theme.Add_theme.
            {
              name = make_e "name";
              description = None;
              color_codes = [ (make_e "color_a", make_e "#fff"); (make_e "color_b", make_e "#000") ];
            }
        in
        let store_theme _ = Lwt.return_ok () in
        let%lwt ret = FL.Theme.(add_theme store_theme command) in
        let expected =
          make ~name:(make_e "name")
            ~colors:
              [
                (make_e "color_a", Color_code.of_string "#fff" |> Option.get);
                (make_e "color_b", Color_code.of_string "#000" |> Option.get);
              ]
            ()
        in
        Alcotest.(check & result theme_t (of_pp FL.Theme.pp_error)) "command" (Ok expected) ret;
        Lwt.return_unit);
    Alcotest_lwt.test_case "return error from workflow to add theme if color code is invalid" `Quick (fun _ () ->
        let command =
          FL.Theme.Add_theme.
            {
              name = make_e "name";
              description = None;
              color_codes = [ (make_e "color_a", make_e "#fff"); (make_e "color_b", make_e "#00") ];
            }
        in
        let store_theme _ = Lwt.return_ok () in
        let%lwt ret = FL.Theme.(add_theme store_theme command) in
        let expected = Error FL.Theme.(Invalid_color_format [ ("color_b", "#00") ]) in
        Alcotest.(check & result theme_t (of_pp FL.Theme.pp_error)) "command" expected ret;
        Lwt.return_unit);
  ]
