let () =
  Alcotest.run "Use cases"
    [ ("use case for planning of filer", Plan_filer_test.test_set)
    ; ("use cases for filer", Filer_test.test_set)
    ; ("use cases for configuration", Configuration_test.test_set) ]
