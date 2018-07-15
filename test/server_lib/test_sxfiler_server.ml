let () =
  let tests = Task.testcases
              @ Rpc_connection.testcases
              @ Task_result_handler.testcases
              @ Scanner_op.testcases
  in
  Alcotest.run "server functionally" tests
