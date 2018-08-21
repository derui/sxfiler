let () =
  let tests = Rpc_connection.testcases
              @ Proc_scanner.testcases
              @ Proc_configuration.testcases
              @ Proc_keymap.testcases
              @ Proc_completion.testcases
              @ Proc_condition.testcases
  in
  Alcotest.run "server functionally" tests
