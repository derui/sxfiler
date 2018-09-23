let () =
  Alcotest.run "Server infrastructures"
    [ ("condition repository", Condition_repo_test.test_set)
    ; ("configuration repository", Configuration_repo_test.test_set)
    ; ("filer repository", Filer_repo_test.test_set)
    ; ("key map respository", Key_map_repo_test.test_set)
    ; ("location scanner service", Location_scanner_service_test.test_set)
    ; ("node transporter service", Node_transporter_service_test.test_set)
    ; ("workbench factory", Workbench_factory_test.test_set)
    ; ("workbench repository", Workbench_repo_test.test_set) ]
