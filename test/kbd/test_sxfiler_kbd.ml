open Mocha_of_ocaml
module K = Sxfiler_kbd

let id x = x

let () =
  "Sxfiler kbd macro" >::: [
    "should parse alphabetical key" >:: (fun _ ->
        let chars = "abcdefghijklmnopqrstuvwxyz"
        and punctures = "-:;=_~^\\/" in
        let upper_chars = chars ^ (String.uppercase_ascii chars) ^ punctures in
        let parameter_set = chars ^ upper_chars ^ punctures in
        let parameter_list = Array.init (String.length parameter_set) id in

        assert_ok (Array.for_all (fun c ->
            let k = String.get parameter_set c |> Char.escaped in
            let t = K.of_keyseq k in
            t = Some {K.empty with key = k}
          ) parameter_list)

      );
    "should parse meta modifier key" >:: (fun _ ->
        let chars = "abcdefghijklmnopqrstuvwxyz"
        and punctures = "-:;=_~^\\/" in
        let upper_chars = chars ^ (String.uppercase_ascii chars) ^ punctures in
        let parameter_set = chars ^ upper_chars ^ punctures in
        let parameter_list = Array.init (String.length parameter_set) id in

        assert_ok (Array.for_all (fun c ->
            let k = String.get parameter_set c |> Char.escaped in
            let t = K.of_keyseq ("M-" ^ k) in
            t = Some {K.empty with key = k; meta = true}
          ) parameter_list)

      );

    "should parse shift key as modifier" >:: (fun _ ->
        let chars = "abcdefghijklmnopqrstuvwxyz"
        and punctures = "-:;=_~^\\/" in
        let upper_chars = chars ^ (String.uppercase_ascii chars) ^ punctures in
        let parameter_set = chars ^ upper_chars ^ punctures in
        let parameter_list = Array.init (String.length parameter_set) id in

        assert_ok (Array.for_all (fun c ->
            let k = String.get parameter_set c |> Char.escaped in
            let t = K.of_keyseq ("S-" ^ k) in
            t = Some {K.empty with key = k; shift = true}
          ) parameter_list)
      );

    "should parse control key as modifier" >:: (fun _ ->
        let chars = "abcdefghijklmnopqrstuvwxyz"
        and punctures = "-:;=_~^\\/" in
        let upper_chars = chars ^ (String.uppercase_ascii chars) ^ punctures in
        let parameter_set = chars ^ upper_chars ^ punctures in
        let parameter_list = Array.init (String.length parameter_set) id in

        assert_ok (Array.for_all (fun c ->
            let k = String.get parameter_set c |> Char.escaped in
            let t = K.of_keyseq ("C-" ^ k) in
            t = Some {K.empty with key = k; ctrl = true}
          ) parameter_list)
      );
  ]
