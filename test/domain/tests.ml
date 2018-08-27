let testcases = [
  "Condition", Condition_test.testcases;
  "keymap", Key_map_test.testcases;
  "filer", Filer_test.testcases;
]

let () =
  Alcotest.run "Domain functionally" testcases
