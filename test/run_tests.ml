let () =
  let open Alcotest in
  run "AllTests"
    [
      ("File_manager", File_manager_tests.all_tests ());
      ("Log_manager", Log_manager_tests.all_tests ());
      ("Lru_replacer", Lru_replacer_tests.all_tests ());
      ("Buffer_manager", Buffer_manager_tests.all_tests ());
      ("Log Records", Log_record_tests.all_tests ());
      ("Transactions", Transaction_tests.all_tests ());
      ("Concurrency", Concurrency_tests.all_tests ());
      ("Record Page", Record_page_tests.all_tests ());
      ("Scan Tests", Scan_tests.all_tests ());
      ("Table Manager", Table_manager_tests.all_tests ());
      ("Parser Test", Parser_tests.all_tests ());
      ("Storage_manager", Storage_manager_tests.all_tests ());
      ("Btree", Btree_tests.all_tests ());
    ]
