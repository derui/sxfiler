let () =
  let tests = Task.testcases
              @ Rpc_connection.testcases
              @ Task_result_handler.testcases
              @ Workspace_op.testcases
  in
  Alcotest.run "server functionally" tests
