let () =
  let open Alcotest in
  run "AllTests"
    [
      ( "File_manager",
        [ test_case "Test 1" `Quick File_manager_tests.test1; 
          test_case "Test 2" `Quick File_manager_tests.test2 ] 
      );
      ("Log_manager", 
        [ test_case "Test 1" `Quick Log_manager_tests.test3 ]);
      ("Buffer", 
        [ test_case "Test 1" `Quick Buffer_manager_tests.test1;
          test_case "Test 2" `Quick Buffer_manager_tests.test2];
      )
    ]

