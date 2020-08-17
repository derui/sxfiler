open Sxfiler_core
module T = Sxfiler_domain.Theme

let test_set =
  let color_t = Alcotest.testable T.Color_code.pp T.Color_code.equal in
  [
    Alcotest_lwt.test_case_sync "Color code: return None when invalid format" `Quick (fun () ->
        Alcotest.(check & option color_t) "empty string" None T.Color_code.(of_string "");
        Alcotest.(check & option color_t) "invalid format" None T.Color_code.(of_string "334433");
        Alcotest.(check & option color_t) "not same hex color for RGB" None T.Color_code.(of_string "#11233");
        Alcotest.(check & option color_t) "not same hex color for RGBA" None T.Color_code.(of_string "#1123344");
        Alcotest.(check & option color_t) "Invalid character" None T.Color_code.(of_string "#AABBgg"));
    Alcotest_lwt.test_case_sync "Color code: return code" `Quick (fun () ->
        let open Option.Infix in
        Alcotest.(check & option string)
          "RGB short notation" (Some "#334455")
          T.Color_code.(of_string "#345" >>| to_string);
        Alcotest.(check & option string)
          "RGBA short notation" (Some "#AABBCC99")
          T.Color_code.(of_string "#abc9" >>| to_string);
        Alcotest.(check & option string)
          "RGB notation" (Some "#112433")
          T.Color_code.(of_string "#112433" >>| to_string);
        Alcotest.(check & option string)
          "RGBA notation" (Some "#11223344")
          T.Color_code.(of_string "#11223344" >>| to_string));
    Alcotest_lwt.test_case_sync "Theme: return theme based on default" `Quick (fun () ->
        let module E = Sxfiler_domain.Common.Not_empty_string in
        let theme =
          T.Definition.make
            ~name:(E.make "name" |> Option.get)
            ~description:(E.make "foo" |> Option.get)
            ~colors:[ (E.make "color" |> Option.get, T.Color_code.of_string "#222" |> Option.get) ]
            ()
        in
        let colors = T.Definition.extract_color theme |> T.Color_map.to_seq |> List.of_seq in
        Alcotest.(check & testable E.pp E.equal) "name of theme" E.(make "name" |> Option.get) theme.name;
        Alcotest.(check string)
          "colors in updated" "#222222"
          T.Color_code.(to_string @@ List.assoc E.(make "color" |> Option.get) colors);
        Alcotest.(check string)
          "colors in default" "#002B36"
          T.Color_code.(to_string @@ List.assoc E.(make "ui.switchRail" |> Option.get) colors));
  ]
