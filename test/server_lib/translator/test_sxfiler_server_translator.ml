let () =
  let suites =
    Completion_test.suite @ Condition_test.suite @ Configuration_test.suite @ File_stat_test.suite
    @ Key_map_test.suite @ Location_history_test.suite @ Location_record_test.suite
    @ Node_test.suite @ Filer_test.suite @ Plan_test.suite
  in
  Alcotest.run "Translators" suites
