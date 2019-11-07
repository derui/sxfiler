let () = Alcotest.run "Server core" [ ("rpc connection", Rpc_connection_test.test_set) ]
