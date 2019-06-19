let () =
  Alcotest.run "Use cases"
    [ ("use cases for filer", Filer_test.test_set)
    ; ("use cases for configuration", Configuration_test.test_set)
    ; ("use cases for key map", Keymap_test.test_set) ]
