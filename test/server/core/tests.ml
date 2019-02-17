let () =
  Alcotest.run "Server core"
    [ ("workbench backend", Workbench_state_test.test_set)
    ; ("rpc connection", Rpc_connection_test.test_set) ]
