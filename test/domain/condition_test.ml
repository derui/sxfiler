module C = Sxfiler_domain.Condition

let test_set =
  [ ( "should empty condition contains empty it"
    , `Quick
    , fun () -> Alcotest.(check bool) "subset" true C.(subset ~current:empty ~parts:empty) )
  ; ( "should return true to compare same condition"
    , `Quick
    , fun () ->
      let expected = C.of_list ["completing"] in
      Alcotest.(check bool) "equal" true C.(equal expected expected) )
  ; ( "should return true if current condition contains parts condition"
    , `Quick
    , fun () ->
      let expected = C.of_list ["completing"] in
      let actual = C.empty in
      Alcotest.(check bool) "equal" true C.(subset ~current:expected ~parts:actual) )
  ; ( "should return what expected condition is contains"
    , `Quick
    , fun () ->
      let expected = C.of_list ["completing"] in
      let actual = C.of_list ["completing"; "file_tree"] in
      Alcotest.(check bool) "subset" true C.(subset ~current:actual ~parts:expected) )
  ; ( "should return false expected condition is not subset of current"
    , `Quick
    , fun () ->
      let expected = C.of_list ["completing"] in
      let actual = C.of_list ["completing"; "file_tree"] in
      Alcotest.(check bool) "subset" false C.(subset ~parts:actual ~current:expected) ) ]
