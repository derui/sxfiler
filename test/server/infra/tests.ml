let () =
  Alcotest.run "Server infrastructures"
    [ ("condition repository", Condition_repo_test.test_set)
    ; ("configuration repository", Configuration_repo_test.test_set)
    ; ("filer repository", Filer_repo_test.test_set)
    ; ("key map repository", Key_map_repo_test.test_set)
    ; ("bookmark repository", Bookmark_repo_test.test_set)
    ; ("location scanner service", Location_scanner_service_test.test_set)
    ; ("progress notification", Message_notification_test.test_set)
    ; ("message notification", Progress_notification_test.test_set)
    ; ("item transporter service", Item_transporter_service_test.test_set)
    ; ("item replication service", Item_replication_service_test.test_set)
    ; ("item trash service", Item_trash_service_test.test_set)
    ; ("key map resolve service", Key_map_resolve_service_test.test_set) ]
