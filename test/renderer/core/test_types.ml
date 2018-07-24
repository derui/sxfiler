open Mocha_of_ocaml
open Sxfiler_renderer_core.Types

let suite () =
  "Renderer core types" >::: [
    "should empty condition contains empty it" >:: (fun () ->
        assert_ok Condition.(subset ~current:empty ~parts:empty)
      );
    "should return true to compare same condition" >:: (fun () ->
        let expected = Condition.of_list [On_completing] in
        assert_ok Condition.(equal expected expected)
      );
    "should return what expected condition is contains" >:: (fun () ->
        let expected = Condition.of_list [On_completing] in
        let actual = Condition.of_list [On_completing;On_file_tree] in

        assert_ok Condition.(subset ~current:actual ~parts:expected)
      );
    "should return false expected condition is not subset of current" >:: (fun () ->
        let expected = Condition.of_list [On_completing] in
        let actual = Condition.of_list [On_completing;On_file_tree] in

        assert_ok @@ not @@ Condition.(subset ~parts:actual ~current:expected)
      );

  ]
