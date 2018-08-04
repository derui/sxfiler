module C = Sxfiler_domain.Condition

let testcases = [
  "should empty condition contains empty it", `Quick, (fun () ->
      Alcotest.(check bool) "subset" true C.(subset ~current:empty ~parts:empty)
    );
  "should return true to compare same condition", `Quick, (fun () ->
      let expected = C.of_list [C.On_completing] in
      Alcotest.(check bool) "equal" true C.(equal expected expected)
    );
  "should return what expected condition is contains", `Quick, (fun () ->
      let expected = C.of_list [On_completing] in
      let actual = C.of_list [On_completing;On_file_tree] in

      Alcotest.(check bool) "subset" true C.(subset ~current:actual ~parts:expected)
    );
  "should return false expected condition is not subset of current", `Quick, (fun () ->
      let expected = C.of_list [On_completing] in
      let actual = C.of_list [On_completing;On_file_tree] in

      Alcotest.(check bool) "subset" false C.(subset ~parts:actual ~current:expected)
    );
]
