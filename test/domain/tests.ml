let () =
  Lwt_main.run
  @@ Alcotest_lwt.run "Domains"
       [
         ("Common", Common_test.test_set);
         ("Context", Context_test.test_set);
         ("File stat", File_stat_test.test_set);
         ("File item", File_item_test.test_set);
         ("File list", File_list_test.test_set);
         ("File window", File_window_test.test_set);
         ("Bookmarks", Bookmarks_test.test_set);
         ("Keymap", Keymap_test.test_set);
         ("Filer", Filer_test.test_set);
         ("Configuration store", Configuration_store_test.test_set);
         ("Theme", Theme_test.test_set);
       ]
