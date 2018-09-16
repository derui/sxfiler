let testcases = Filer_test.testcases @ Plan_filer_test.testcases
let run () = Alcotest.run "usecases" testcases
