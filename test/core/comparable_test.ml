open Sxfiler_core

let tests =
  [
    ( "allow to compare values with provided function from type",
      `Quick,
      fun () ->
        let module S = Comparable.Make (struct
          type t = float

          let compare = Float.compare
        end) in
        Alcotest.(check int) "compare" 1 S.(compare 2. 1.) );
    ( "allow to use infix operators",
      `Quick,
      fun () ->
        let module S = Comparable.Make (struct
          type t = float

          let compare = Float.compare
        end) in
        Alcotest.(check bool) "equal" true S.Infix.(1. = 1.);
        Alcotest.(check bool) "gt" true S.Infix.(2. > 1.);
        Alcotest.(check bool) "gt" false S.Infix.(1. > 1.);
        Alcotest.(check bool) "lt" true S.Infix.(1. < 2.);
        Alcotest.(check bool) "lt" false S.Infix.(1. < 1.);
        Alcotest.(check bool) "ge" true S.Infix.(2. >= 1.);
        Alcotest.(check bool) "ge" true S.Infix.(1. >= 1.);
        Alcotest.(check bool) "le" true S.Infix.(1. <= 2.);
        Alcotest.(check bool) "le" true S.Infix.(1. <= 1.);
        Alcotest.(check bool) "not equal" true S.Infix.(2. <> 1.) );
  ]
