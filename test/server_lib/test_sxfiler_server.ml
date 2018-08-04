let () =
  let tests = Task.testcases
              @ Rpc_connection.testcases
              @ Task_result_handler.testcases
              @ Proc_scanner.testcases
              @ Proc_configuration.testcases
              @ Proc_keymap.testcases
              @ Proc_completion.testcases
  in
  Alcotest.run "server functionally" tests
