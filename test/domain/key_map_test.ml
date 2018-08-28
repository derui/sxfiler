module Kbd = Sxfiler_kbd
module C = Sxfiler_domain.Condition
module K = Sxfiler_domain.Key_map

let testcases =
  [ ( "add and find action bound with key"
    , `Quick
    , fun () ->
      let t = K.make () in
      let t = K.add t ~condition:C.empty ~key:(Kbd.make "k") ~value:"foo" in
      Alcotest.(check @@ option string)
        "find" (Some "foo")
        K.(find t ~condition:C.empty ~key:(Kbd.make "k")) )
  ; ( "can add same key binding with another condition"
    , `Quick
    , fun () ->
      let t = K.make () in
      let cond = C.(empty |> enable ~context:"foo")
      and cond' = C.(empty |> enable ~context:"bar")
      and key = Kbd.make "k" in
      let t = K.add t ~condition:cond ~key ~value:"foo" in
      let t = K.add t ~condition:cond' ~key ~value:"bar" in
      Alcotest.(check @@ option string) "find" (Some "foo") K.(find t ~condition:cond ~key) ;
      Alcotest.(check @@ option string) "find" (Some "bar") K.(find t ~condition:cond' ~key) )
  ; ( "get subset of key map with condition"
    , `Quick
    , fun () ->
      let t = K.make () in
      let cond = C.(empty |> enable ~context:"foo")
      and cond' = C.(empty |> enable ~context:"bar")
      and key = Kbd.make "k" in
      let t =
        t |> K.add ~condition:cond ~key ~value:"foo" |> K.add ~condition:cond' ~key ~value:"bar"
      in
      let t' = K.subset t ~condition:cond in
      Alcotest.(check @@ option string) "find" (Some "foo") K.(find t' ~condition:cond ~key) ;
      Alcotest.(check @@ option string) "find" None K.(find t' ~condition:cond' ~key) ) ]
