let () =
  let tests =
    Completion.testcases
    @ Configuration.testcases
    @ Keymap.testcases
    @ Scanner.testcases
  in

  Alcotest.run "gateways" tests
