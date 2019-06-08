module Kbd = Sxfiler_kbd
module C = Sxfiler_domain.Condition
module K = Sxfiler_domain.Key_map

let test_set =
  [ ( "can add same key binding with another contexts"
    , `Quick
    , fun () ->
        let t = K.make () in
        let key = Kbd.make "k" in
        let t = K.add t ~contexts:["foo"] ~key ~value:"foo" in
        let t = K.add t ~contexts:["bar"] ~key ~value:"bar" in
        let bindings =
          K.bindings t
          |> List.sort (fun (c1, _, _) (c2, _, _) -> compare c1 c2)
          |> List.map (fun (c, _, v) -> (c, v))
        in
        Alcotest.(check @@ list @@ pair (list string) string)
          "find"
          [(["bar"], "bar"); (["foo"], "foo")]
          bindings )
  ; ( "can remove key binding "
    , `Quick
    , fun () ->
        let t = K.make () in
        let key = Kbd.make "k" in
        let t =
          K.add t ~contexts:["foo"] ~key ~value:"foo"
          |> K.add ~contexts:["bar"] ~key ~value:"bar"
          |> K.remove ~contexts:["foo"] ~key
        in
        let bindings =
          K.bindings t
          |> List.sort (fun (c1, _, _) (c2, _, _) -> compare c1 c2)
          |> List.map (fun (c, _, v) -> (c, v))
        in
        Alcotest.(check @@ list @@ pair (list string) string) "find" [(["bar"], "bar")] bindings )
  ]
