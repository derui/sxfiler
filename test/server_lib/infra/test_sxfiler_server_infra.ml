let () =
  let tests =
    Node_repo.suite @ Key_map_repo.suite @ Configuration_repo.suite @ Filer_repo.suite
    @ Condition_repo.suite
  in
  Alcotest.run "Sxfiler server infra" tests
