let () =
  let open Alcotest in
  run "AllTests"
    [
      ("File_manager", File_manager_tests.all_tests ());
      ("Log_manager", Log_manager_tests.all_tests ());
      ("Buffer_manager", Buffer_manager_tests.all_tests ());
      ("Log Records", Log_record_tests.all_tests ());
    ]
