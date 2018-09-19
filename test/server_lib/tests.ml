let () =
  Alcotest.run "server library"
    [ ("completion procedures", Proc_completion_test.test_set)
    ; ("configuration procedures", Proc_configuration_test.test_set)
    ; ("filer procedures", Proc_filer_test.test_set)
    ; ("key map procedures", Proc_keymap_test.test_set)
    ; ("rpc connection", Rpc_connection_test.test_set) ]
