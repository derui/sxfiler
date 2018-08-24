let () =
  let tests =
    Completion_test.testcases
    @ Configuration_test.testcases
    @ Keymap_test.testcases
    @ Filer_test.testcases
  in

  Alcotest.run "gateways" tests
