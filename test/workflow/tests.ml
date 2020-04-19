let () =
  Lwt_main.run
  @@ Alcotest_lwt.run "Workflow"
       [
         ("File list steps", Common_step_file_list_test.test_set);
         ("Filer steps", Common_step_filer_test.test_set);
         ("Completer steps", Common_step_completer_test.test_set);
         ("Filer work flows", Filer_test.test_set);
         ("Keymap work flows", Keymap_test.test_set);
         ("Configuration work flows", Configuration_test.test_set);
       ]
