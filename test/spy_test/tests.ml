let single_arg_test_set =
  [ ( "wrapped pure function can call no change before"
    , `Quick
    , fun () ->
        let _, wrapped = Spy.wrap succ in
        Alcotest.(check int) "wrapped" 2 (wrapped 1) ;
        Alcotest.(check int) "wrapped" 3 (wrapped 1 |> wrapped) )
  ; ( "extract arguments called after wrapped"
    , `Quick
    , fun () ->
        let t, wrapped = Spy.wrap succ in
        wrapped 1 |> ignore ;
        wrapped 2 |> wrapped |> ignore ;
        Alcotest.(check @@ of_pp Fmt.nop) "wrapped" [1; 2; 3] (Spy.Wrap.called_args t) )
  ; ( "wrapped impure function can call no change before"
    , `Quick
    , fun () ->
        let t, wrapped = Spy.wrap print_string in
        wrapped "foo" ;
        wrapped "bar" ;
        Alcotest.(check @@ list string) "impure" ["foo"; "bar"] (Spy.Wrap.called_args t) )
  ; ( "wrap curried function"
    , `Quick
    , fun () ->
        let t, wrapped = List.iter print_string |> Spy.wrap in
        wrapped ["foo"; "bar"] ;
        wrapped ["one"; "two"] ;
        Alcotest.(check @@ list @@ list string)
          "curried" [["foo"; "bar"]; ["one"; "two"]] (Spy.Wrap.called_args t) ) ]

let multiple_arg_test_set =
  [ ( "wrap multiple arguments"
    , `Quick
    , fun () ->
        let t, f = Spy.wrap2 ( + ) in
        let ret = f 1 2 in
        Alcotest.(check int) "wrapped" 3 ret ;
        Alcotest.(check @@ list @@ pair int int) "called" [(1, 2)] (Spy.Wrap2.called_args t) ) ]

let () =
  Alcotest.run "spy"
    [("single argument", single_arg_test_set); ("multiple arguments", multiple_arg_test_set)]
