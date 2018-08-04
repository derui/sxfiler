let testcases = [
  "Condition", Condition_test.testcases;
]

let () =
  Alcotest.run "Domain functionally" testcases
