module K = Sxfiler_kbd

let test_set =
  let key_test = Alcotest.testable K.pp K.equal in
  [
    ( "Make key from constructor",
      `Quick,
      fun () ->
        let key = K.make "k" in
        Alcotest.(check bool) "meta" false (K.is_meta_pressed key);
        Alcotest.(check bool) "ctrl" false (K.is_ctrl_pressed key);
        Alcotest.(check string) "key" "k" (K.key key) );
    ( "Make key with meta and ctrl",
      `Quick,
      fun () ->
        let meta_key = K.make ~meta:true "k" and ctrl_key = K.make ~ctrl:true "v" in
        Alcotest.(check bool) "meta" true (K.is_meta_pressed meta_key);
        Alcotest.(check bool) "ctrl" true (K.is_ctrl_pressed ctrl_key) );
    ( "Can create from key-combination string",
      `Quick,
      fun () ->
        let key_expected = K.make "K" |> Option.some
        and meta_key_expected = K.make ~meta:true "v" |> Option.some
        and ctrl_key_expected = K.make ~ctrl:true "c" |> Option.some
        and all_key_expected = K.make ~meta:true ~ctrl:true "a" |> Option.some in
        Alcotest.(check @@ option key_test) "key only" key_expected (K.of_keyseq "K");
        Alcotest.(check @@ option key_test) "with ctrl" ctrl_key_expected (K.of_keyseq "C-c");
        Alcotest.(check @@ option key_test) "with meta" meta_key_expected (K.of_keyseq "M-v");
        Alcotest.(check @@ option key_test) "all key" all_key_expected (K.of_keyseq "C-M-a");
        Alcotest.(check @@ option key_test) "swapped" all_key_expected (K.of_keyseq "M-C- a") );
    ( "Should return None if illegal key sequence",
      `Quick,
      fun () ->
        Alcotest.(check @@ option key_test) "empty" None (K.of_keyseq "");
        Alcotest.(check @@ option key_test) "illegal meta key" None (K.of_keyseq "-c");
        Alcotest.(check @@ option key_test) "no key" None (K.of_keyseq "M-") );
    ( "compare key sequence",
      `Quick,
      fun () ->
        let key_only = K.make "K"
        and with_meta = K.make ~meta:true "K"
        and with_ctrl = K.make ~ctrl:true "K"
        and all_meta = K.make ~meta:true ~ctrl:true "K"
        and next_key = K.make "L" in
        Alcotest.(check int) "with meta" 1 (K.compare with_meta key_only);
        Alcotest.(check int) "with ctrl" 1 (K.compare with_ctrl key_only);
        Alcotest.(check int) "all meta" 1 (K.compare all_meta with_ctrl);
        Alcotest.(check int) "all meta" 1 (K.compare all_meta with_meta);
        Alcotest.(check int) "next key" 1 (K.compare next_key all_meta) );
  ]

let () = Alcotest.run "Kbd" [ ("Key sequence", test_set) ]
