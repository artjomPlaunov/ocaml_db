module Test_utils = struct
  open File
  open Storage_manager

  let setup_test_env () =
    let file_manager = File_manager.make ~db_dirname:"tmp_btree_test" ~block_size:128 in
    let storage_manager = Storage_manager.make ~file_manager ~storage_file:"test_btree" in
    storage_manager
end

module Empty_btree_tests = struct
  open Btree
  open Test_utils

  let expected_output = " "

  let run_test () =
    let storage_manager = setup_test_env () in


    let key_type = TVarchar 10 in
    let t = Btree.empty storage_manager key_type in
    Btree.print_node t.root;
    
    let _ = Btree.insert_in_leaf t 1 (Btree.empty_key key_type) "str" in 

    ""

  let test () =
    Alcotest.(check string)
      "verify empty btree creation"
      expected_output
      (run_test ())
end

let all_tests () =
  [
    Alcotest.test_case "empty btree creation" `Quick Empty_btree_tests.test
  ]