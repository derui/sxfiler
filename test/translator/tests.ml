let () =
  Lwt_main.run
  @@ Alcotest_lwt.run "translators"
       [
         ("configuration store translator", Configuration_store_test.test_set);
         ("completer translator", Completer_test.test_set);
         ("context translator", Context_test.test_set);
         ("file_stat translator", File_stat_test.test_set);
         ("file_item translator", File_item_test.test_set);
         ("file_list translator", File_list_test.test_set);
         ("location_history translator", Location_history_test.test_set);
         ("keymap translator", Keymap_test.test_set);
         ("filer translator", Filer_test.test_set);
       ]
