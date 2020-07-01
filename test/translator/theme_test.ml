module D = Sxfiler_domain
module T = Sxfiler_translator.Theme
module G = Sxfiler_generated.Theme

let test_set =
  let string v = D.Common.Not_empty_string.make v |> Option.get in
  let color_code v = D.Theme.Color_code.of_string v |> Option.get in
  let theme_t = Alcotest.testable D.Theme.pp D.Theme.equal in
  [
    Alcotest_lwt.test_case_sync "can translate to/from domain" `Quick (fun () ->
        let name = string "name of theme" in
        let colors = [ (string "key", color_code "#ffaabb"); (string "key2", color_code "#a0b") ] in
        let theme = D.Theme.make ~name ~colors () in
        Alcotest.(check @@ result theme_t @@ of_pp T.pp_error) "domain" (Ok theme) (T.to_domain @@ T.of_domain theme));
    Alcotest_lwt.test_case_sync "return error when name is empty" `Quick (fun () ->
        let data = { G.Theme.name = ""; description = ""; color_codes = [] } in
        Alcotest.(check @@ result theme_t @@ of_pp T.pp_error) "domain" (Error T.Empty_name) (T.to_domain data));
    Alcotest_lwt.test_case_sync "return error when color key is empty" `Quick (fun () ->
        let data =
          { G.Theme.name = "name"; description = ""; color_codes = [ { G.ColorCode.name = ""; hex_color = "#fff" } ] }
        in
        Alcotest.(check @@ result theme_t @@ of_pp T.pp_error) "domain" (Error T.Empty_color_key) (T.to_domain data));
    Alcotest_lwt.test_case_sync "return error when color code is invalid" `Quick (fun () ->
        let data =
          {
            G.Theme.name = "name";
            description = "";
            color_codes = [ { G.ColorCode.name = "name"; hex_color = "#Gff" } ];
          }
        in
        Alcotest.(check @@ result theme_t @@ of_pp T.pp_error)
          "domain" (Error (T.Invalid_color_code "#Gff")) (T.to_domain data));
  ]
