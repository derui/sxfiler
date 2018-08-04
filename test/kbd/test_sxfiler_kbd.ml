open Mocha_of_ocaml
module K = Sxfiler_kbd
module Kj = Sxfiler_kbd_jsoo

let id x = x

let alphabetical_keys =
  let chars = "abcdefghijklmnopqrstuvwxyz"
  and punctures = "-:;=_~^\\/" in
  let upper_chars = chars ^ (String.uppercase_ascii chars) ^ punctures in
  chars ^ upper_chars ^ punctures

let () =
  "Sxfiler kbd macro" >::: [
    "should convert between Javascript object and kbd type" >:: (fun _ ->
        let k = {K.ctrl = true; meta = false; key = "k"} in
        let js = Kj.to_js k in
        let json : Kj.js Js.t = Js._JSON##stringify js |> fun s -> Js._JSON##parse s in
        let conv_k = Kj.of_js json in

        assert_ok (k = conv_k);
      );
    "should convert kbd type to string" >:: (fun _ ->
        let k = {K.ctrl = true; meta = false; key = "k"} in
        let js = K.to_keyseq k in

        assert_ok (js = "C-k");
      );
    "should not equal key sequence and converted key sequence" >:: (fun _ ->
        let k = {K.ctrl = true; meta = false; key = "k"} in
        let seq = K.to_keyseq k in
        let seq_from_string = match K.of_keyseq "C-k" with
          | Some k -> K.to_keyseq k
          | None -> ""
        in

        let open Infix in
        assert_ok (seq = seq_from_string) <|> assert_neq seq_from_string "C-k";
      );
    "should be able to convert special key name" >:: (fun _ ->
        let k = {K.ctrl = true; meta = false; key = "Tab"} in
        let js = Kj.to_js k in
        let json : Kj.js Js.t = Js._JSON##stringify js |> fun s -> Js._JSON##parse s in
        let conv_k = Kj.of_js json in

        assert_ok (k = conv_k);
      );
    "should parse alphabetical key" >:: (fun _ ->
        let parameter_list = Array.init (String.length alphabetical_keys) id in

        assert_ok (Array.for_all (fun c ->
            let k = String.get alphabetical_keys c |> String.make 1 in
            let t = K.of_keyseq k in
            t = Some {K.empty with key = k}
          ) parameter_list)

      );
    "should parse meta modifier key" >:: (fun _ ->
        let parameter_list = Array.init (String.length alphabetical_keys) id in

        assert_ok (Array.for_all (fun c ->
            let k = String.get alphabetical_keys c |> String.make 1 in
            let t = K.of_keyseq ("M-" ^ k) in
            t = Some {K.empty with key = k; meta = true}
          ) parameter_list)

      );

    "should parse control key as modifier" >:: (fun _ ->
        let parameter_list = Array.init (String.length alphabetical_keys) id in

        assert_ok (Array.for_all (fun c ->
            let k = String.get alphabetical_keys c |> String.make 1 in
            let t = K.of_keyseq ("C-" ^ k) in
            t = Some {K.empty with key = k; ctrl = true}
          ) parameter_list)
      );

    "should return None only prefix" >:: (fun _ ->
        let parameter = ["M-";"C-"] in
        assert_ok @@ List.for_all (fun p -> let t = K.of_keyseq p in t = None) parameter
      );

    "should be able to contain multi prefix" >:: (fun _ ->
        let has_meta k = k.K.meta
        and has_ctrl k = k.K.ctrl in
        let ( >>> ) v k = (fst v ^ fst k, fun key -> snd v key && snd k key) in
        let meta = ("M-", has_meta)
        and ctrl = ("C-", has_ctrl) in

        let prefix_patterns = [
          meta; ctrl;
          meta >>> ctrl;
          ctrl >>> meta;
        ] in
        let parameter_list = Array.init (String.length alphabetical_keys) id |> Array.to_list in

        assert_ok @@ List.for_all (fun k ->
            List.for_all (fun (prefix, validator) ->
                let k = String.get alphabetical_keys k in
                let seq = prefix ^ (String.make 1 k) in
                match K.of_keyseq seq with
                | None -> false
                | Some t -> validator t && (t.K.key = (String.make 1 k))
              ) prefix_patterns
          ) parameter_list
      );
  ]
