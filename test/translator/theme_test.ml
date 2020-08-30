module D = Sxfiler_domain
module T = Sxfiler_translator.Theme
module G = Sxfiler_generated.Theme

let test_set =
  let string v = D.Common.Not_empty_string.make v |> Option.get in
  let color_code v = D.Theme.Color_code.of_string v |> Option.get in
  let color_code_t = Alcotest.testable D.Theme.Color_code.pp D.Theme.Color_code.equal in
  let empty_t = Alcotest.testable D.Common.Not_empty_string.pp D.Common.Not_empty_string.equal in
  let pair_t = Alcotest.pair empty_t color_code_t in
  [
    Alcotest_lwt.test_case_sync "can translate to/from domain" `Quick (fun () ->
        let sort_colors = List.sort (fun v1 v2 -> D.Common.Not_empty_string.compare (fst v1) (fst v2)) in
        let colors = [ (string "key", color_code "#ffaabb"); (string "key2", color_code "#a0b") ] in
        let colors = D.Theme.Configuration.(make ~colors () |> merge_color) |> sort_colors in

        Alcotest.(check @@ result (list pair_t) @@ of_pp T.pp_error)
          "domain" (Ok colors)
          (T.to_domain @@ T.of_domain colors |> Result.map sort_colors));
    Alcotest_lwt.test_case_sync "return error when color key is empty" `Quick (fun () ->
        let data = { G.ColorTheme.color_pairs = [ { G.ColorPair.name = ""; hex_color = "#fff" } ] } in
        Alcotest.(check @@ result (list pair_t) @@ of_pp T.pp_error)
          "domain" (Error T.Empty_color_key) (T.to_domain data));
    Alcotest_lwt.test_case_sync "return error when color code is invalid" `Quick (fun () ->
        let data = { G.ColorTheme.color_pairs = [ { G.ColorPair.name = "name"; hex_color = "#Gff" } ] } in
        Alcotest.(check @@ result (list pair_t) @@ of_pp T.pp_error)
          "domain" (Error (T.Invalid_color_code "#Gff")) (T.to_domain data));
  ]
