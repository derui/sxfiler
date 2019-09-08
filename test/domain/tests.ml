let () =
  Alcotest.run "Domains"
    [ ("condition", Condition_test.test_set)
    ; ("filer", Filer_test.test_set)
    ; ("key map", Key_map_test.test_set)
    ; ("task", Task_test.test_set)
    ; ("bookmark", Bookmark_test.test_set) ]
