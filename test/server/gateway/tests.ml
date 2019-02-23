let () =
  Alcotest.run "gateways"
    [ ("completion gateway", Completion_test.test_set)
    ; ("configuration gateway", Configuration_test.test_set)
    ; ("Key map gateway", Keymap_test.test_set)
    ; ("filer gateway", Filer_test.test_set)
    ; ("plan gateway", Plan_test.test_set)
    ; ("notification gateway", Notification_test.test_set) ]