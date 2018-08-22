let () =
  let tests = Rpc_connection.testcases
              @ Proc_scanner.testcases
              @ Proc_configuration.testcases
              @ Proc_keymap.testcases
              @ Proc_completion.testcases
  in
  Alcotest.run "server functionally" tests
