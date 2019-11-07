let () =
  Alcotest.run "translators"
    [
      ("completion translator", Completion_test.test_set);
      ("condition translator", Condition_test.test_set);
      ("configuration translator", Configuration_test.test_set);
      ("file_stat translator", File_stat_test.test_set);
      ("filer translator", Filer_test.test_set);
      ("key map translator", Key_map_test.test_set);
      ("location history translator", Location_history_test.test_set);
      ("location record translator", Location_record_test.test_set);
      ("file_item translator", File_item_test.test_set);
      ("task interaction translator", Task_interaction_test.test_set);
      ("bookmark translator", Bookmark_test.test_set);
    ]
