module Test_utils = struct
  open File
  open Storage_manager

  let write_output_to_file output =
    let out_file = Filename.concat "tmp_btree_test" "test_output.txt" in
    let oc = open_out out_file in
    output_string oc output;
    close_out oc

  let print_storage_contents sm =
    let fm = sm.file_manager in
    let block_size = File_manager.get_blocksize fm in
    let sfile = sm.storage_file in
    let size = File_manager.size fm sfile in
    let output = Buffer.create 256 in
    Buffer.add_string output "Storage contents:\n";
    for i = 0 to size - 1 do
      let block = Block_id.make ~filename:sfile ~block_num:i in
      let page = Page.make ~block_size in
      File_manager.read fm block page;
      let value = Int32.to_int (Page.get_int32 page 0) in
      Buffer.add_string output (Printf.sprintf "Block %d: %d\n" i value)
    done;
    Buffer.contents output

  let setup_test_env () =
    let file_manager = File_manager.make ~db_dirname:"tmp_btree_test" ~block_size:128 in
    let storage_manager = Storage_manager.make ~file_manager ~storage_file:"test_btree" in
    storage_manager
end

module Empty_btree_tests = struct
  open Btree
  open Test_utils

  let expected_output = "Initial storage state:
Storage contents:
Block 0: 0

After creating empty btree:
Storage contents:
Block 0: 0
Block 1: -1431655766
"

  let run_test () =
    let storage_manager = setup_test_env () in
    let output = Buffer.create 256 in
    
    Buffer.add_string output "Initial storage state:\n";
    Buffer.add_string output (print_storage_contents storage_manager);
    
    let key_type = TVarchar 10 in
    let _ = Btree.empty storage_manager key_type in
    
    Buffer.add_string output "\nAfter creating empty btree:\n";
    Buffer.add_string output (print_storage_contents storage_manager);
    
    let final_output = Buffer.contents output in
    write_output_to_file final_output;
    final_output

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