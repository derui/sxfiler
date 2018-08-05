open Sxfiler_core

module D = Sxfiler_domain
module T = Sxfiler_server_translator.Key_map

let data = List.fold_left (fun keymap (key, value) ->
    D.Key_map.add ~condition:D.Condition.empty ~key ~value keymap
  )
    (D.Key_map.make ())
    [Sxfiler_kbd.make "j", "foo";
     Sxfiler_kbd.make "k", "bar";
    ]

let testcases = [
  "can translate to/from domain", `Quick, (fun () ->
      Alcotest.(check @@ of_pp Fmt.nop) "domain" data (T.to_domain @@ T.of_domain data)
    );
  "can translate to/from yojson", `Quick, (fun () ->
      let data = T.of_domain data in
      Alcotest.(check @@ result (of_pp Fmt.nop) (of_pp Fmt.nop)) "yojson"
        (Ok data) (T.of_yojson @@ T.to_yojson data)
    );
]

let suite = [
  "key_map", testcases;
]
