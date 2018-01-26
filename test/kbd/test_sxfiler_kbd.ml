open Mocha_of_ocaml
module K = Sxfiler_kbd

let id x = x

let alphabetical_keys =
  let chars = "abcdefghijklmnopqrstuvwxyz"
  and punctures = "-:;=_~^\\/" in
  let upper_chars = chars ^ (String.uppercase_ascii chars) ^ punctures in
  chars ^ upper_chars ^ punctures

let () =
  "Sxfiler kbd macro" >::: [
    "should parse alphabetical key" >:: (fun _ ->
        let parameter_list = Array.init (String.length alphabetical_keys) id in

        assert_ok (Array.for_all (fun c ->
            let k = String.get alphabetical_keys c |> String.make 1 in
            let t = K.of_keyseq k in
            if t <> Some {K.empty with key = k} then
              Firebug.console##log t;

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

    "should parse shift key as modifier" >:: (fun _ ->
        let parameter_list = Array.init (String.length alphabetical_keys) id in

        assert_ok (Array.for_all (fun c ->
            let k = String.get alphabetical_keys c |> String.make 1 in
            let t = K.of_keyseq ("S-" ^ k) in
            t = Some {K.empty with key = k; shift = true}
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
        let parameter = ["M-";"C-";"S-"] in
        assert_ok @@ List.for_all (fun p -> let t = K.of_keyseq p in t = None) parameter
      );

    "should be able to contain multi prefix" >:: (fun _ ->
        let has_meta k = k.K.meta
        and has_ctrl k = k.K.ctrl
        and has_shift k = k.K.shift in
        let ( >>> ) v k = (fst v ^ fst k, fun key -> snd v key && snd k key) in
        let meta = ("M-", has_meta)
        and ctrl = ("C-", has_ctrl)
        and shift = ("S-", has_shift) in

        let prefix_patterns = [
          meta; ctrl; shift;
          meta >>> shift;
          meta >>> ctrl;
          shift >>> meta;
          shift >>> ctrl;
          ctrl >>> meta;
          ctrl >>> shift;
          meta >>> ctrl >>> shift;
          meta >>> shift >>> ctrl;
          ctrl >>> meta >>> shift;
          ctrl >>> shift >>> meta;
          shift >>> meta >>> ctrl;
          shift >>> ctrl >>> meta;
        ] in
        let parameter_list = Array.init (String.length alphabetical_keys) id |> Array.to_list in

        assert_ok @@ List.for_all (fun k ->
            List.for_all (fun (prefix, validator) ->
                let k = String.get alphabetical_keys k in
                let seq = prefix ^ (String.make 1 k) in
                match K.of_keyseq seq with
                | None -> false
                | Some t -> Firebug.console##log t; validator t && (t.K.key = (String.make 1 k))
              ) prefix_patterns
          ) parameter_list
      );
  ]
