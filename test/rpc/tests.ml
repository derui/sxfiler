let () =
  Lwt_main.run
  @@ Alcotest_lwt.run "Rpc"
       [
         ("Interaction mediator", Interaction_mediator_test.test_set);
         ("Command client", Client_test.test_set);
         ("Command server", Server_test.test_set);
         ("Filer endpoint", Filer_endpoint_test.test_set);
         ("Keymap endpoint", Keymap_endpoint_test.test_set);
         ("Configuration endpoint", Configuration_endpoint_test.test_set);
       ]
