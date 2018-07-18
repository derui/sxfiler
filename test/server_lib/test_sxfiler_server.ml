let () =
  let tests = Task.testcases
              @ Rpc_connection.testcases
              @ Task_result_handler.testcases
              @ Proc_scanner.testcases
  in
  Alcotest.run "server functionally" tests
