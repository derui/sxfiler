let () =
  Lwt_main.run
  @@ Alcotest_lwt.run "Server infrastructures"
       [
         ("Filer step implementation", Filer_step_test.test_set);
         ("File_list step implementation", File_list_step_test.test_set);
         ("Keymap step implementation", Keymap_step_test.test_set);
         ("Forward match completer implementation", Forward_match_completer_test.test_set);
         ("Migemo completer implementation", Migemo_completer_test.test_set);
         ("websocket connection wrapper", Ws_connection_test.test_set);
         ("websocket actor", Ws_actor_test.test_set);
       ]
