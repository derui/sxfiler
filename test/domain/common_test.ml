module D = Sxfiler_domain

let test_set =
  [
    Alcotest_lwt.test_case_sync "make from non-empty string" `Quick (fun () ->
        let expected = D.Common.Not_empty_string.make "foo" |> Option.get in
        Alcotest.(check @@ option @@ of_pp D.Common.Not_empty_string.pp)
          "non-empty" (Some expected) (D.Common.Not_empty_string.make "foo"));
    Alcotest_lwt.test_case_sync "raise error when string is empty" `Quick (fun () ->
        Alcotest.(check @@ option @@ of_pp D.Common.Not_empty_string.pp)
          "non-empty" None (D.Common.Not_empty_string.make ""));
    Alcotest_lwt.test_case_sync "make from positive integer" `Quick (fun () ->
        let expected = D.Common.Positive_number.make 15 |> Option.get in
        Alcotest.(check @@ option @@ of_pp D.Common.Positive_number.pp)
          "positive" (Some expected) (D.Common.Positive_number.make 15);
        Alcotest.(check int) "value" 15 (D.Common.Positive_number.value expected));
    Alcotest_lwt.test_case_sync "raise error if number is not positive" `Quick (fun () ->
        Alcotest.(check @@ option @@ of_pp D.Common.Positive_number.pp) "zero" None (D.Common.Positive_number.make 0);
        Alcotest.(check @@ option @@ of_pp D.Common.Positive_number.pp)
          "minus" None (D.Common.Positive_number.make (-1)));
  ]
