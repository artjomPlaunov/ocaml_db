module To_test = struct
  open File

  let test1 =
    let file_manager =
      File_manager.make ~db_dirname:"buffertest1" ~block_size:512
    in
    let log_file = "buffertest_log" in
    let log_manager = Log_manager.make ~file_manager ~log_file in
    let buffer_manager =
      Buffer_manager.make ~file_manager ~log_manager ~num_buffers:1 ()
    in
    let block1 = File.Block_id.make ~filename:"testfile" ~block_num:1 in
    let block2 = File.Block_id.make ~filename:"testfile" ~block_num:2 in
    let buf1 = Buffer_manager.pin buffer_manager block1 in
    let page = Buffer_manager__Db_buffer.contents buf1 in
    let n = Int32.to_int (File.Page.get_int32 page 80) in
    Page.set_int32 page 80 (Int32.of_int (n + 1));
    Buffer_manager__Db_buffer.set_modified buf1 1 0;
    Buffer_manager.unpin buffer_manager buf1;
    let buf2 = Buffer_manager.pin buffer_manager block2 in
    Test_utils.no_diff "buffertest1/testfile" "buffer_manager_output/test1.txt"

  let test2 =
    let file_manager =
      File.File_manager.make ~db_dirname:"buffertest2" ~block_size:512
    in
    let log_file = "buffertest_log" in
    let log_manager = Log_manager.make ~file_manager ~log_file in
    let buffer_manager =
      Buffer_manager.make ~file_manager ~log_manager ~num_buffers:3 ()
    in
    let block1 = Block_id.make ~filename:"testfile" ~block_num:0 in
    let buf1 = Buffer_manager.pin buffer_manager block1 in
    let p = Buffer_manager__Db_buffer.contents buf1 in
    let n = Int32.to_int (File.Page.get_int32 p 0) in
    Page.set_int32 p 0 (Int32.of_int (n + 1));
    Buffer_manager__Db_buffer.set_modified buf1 1 0;
    Buffer_manager.unpin buffer_manager buf1;
    let block2 = Block_id.make ~filename:"testfile" ~block_num:100 in
    let block3 = Block_id.make ~filename:"testfile" ~block_num:3 in
    let block4 = Block_id.make ~filename:"testfile" ~block_num:4 in
    let buf2 = Buffer_manager.pin buffer_manager block2 in
    let p2 = Buffer_manager__Db_buffer.contents buf2 in
    let n = Int32.to_int (File.Page.get_int32 p2 0) in
    let buf3 = Buffer_manager.pin buffer_manager block3 in
    let buf4 = Buffer_manager.pin buffer_manager block4 in
    Buffer_manager.unpin buffer_manager buf2;

    let buf2 = Buffer_manager.pin buffer_manager block1 in
    let p2 = Buffer_manager__Db_buffer.contents buf2 in
    Page.set_int32 p2 80 (Int32.of_int 9999);
    Buffer_manager__Db_buffer.set_modified buf2 1 0;
    Buffer_manager.unpin buffer_manager buf2;
    Test_utils.no_diff "buffertest2/testfile" "buffer_manager_output/test2.txt"

  (* Buffer_manager test. *)
  let test3 =
    let file_manager =
      File.File_manager.make ~db_dirname:"buffertest3" ~block_size:512
    in
    let log_file = "buffer_manager_test_log" in
    let log_manager = Log_manager.make ~file_manager ~log_file in
    let buffer_manager =
      Buffer_manager.make ~file_manager ~log_manager ~num_buffers:3 ()
    in
    let block0 = Block_id.make ~filename:"testfile" ~block_num:0 in
    let block1 = Block_id.make ~filename:"testfile" ~block_num:1 in
    let block2 = Block_id.make ~filename:"testfile" ~block_num:2 in
    let block3 = Block_id.make ~filename:"testfile" ~block_num:3 in
    let block4 = Block_id.make ~filename:"testfile" ~block_num:4 in
    let block5 = Block_id.make ~filename:"testfile" ~block_num:5 in
    (* Pin blocks 0, 1, 2 *)
    let buf0 = Buffer_manager.pin buffer_manager block0 in
    let buf1 = Buffer_manager.pin buffer_manager block1 in
    let buf2 = Buffer_manager.pin buffer_manager block2 in
    (* Buffer is full. Unpin block 1. *)
    Buffer_manager.unpin buffer_manager buf1;
    (* Buffer has an empty space. *)
    (* Pin block 0 again (already pinned). *)
    let buf3 = Buffer_manager.pin buffer_manager block0 in
    (* Pin block 1 again. Buffer is now full again. *)
    let buf4 = Buffer_manager.pin buffer_manager block1 in
    let s1 = Printf.sprintf "Attempting to pin block 3...\n" in
    let s2 =
      try
        let _ = Buffer_manager.pin buffer_manager block3 in
        "test fail"
      with Buffer_manager.BufferAbortException ->
        Printf.sprintf "Exception: No available Buffers"
    in
    Buffer_manager.unpin buffer_manager buf2;
    (* Now we should be able to pin block3, after unpinning block2. *)
    let buf5 = Buffer_manager.pin buffer_manager block3 in
    s1 ^ s2
end



let test1 () = Alcotest.(check bool) "bool equality" true To_test.test1
let test2 () = Alcotest.(check bool) "bool equality" true To_test.test2
let test3 () = Alcotest.(check string) "same string" "Attempting to pin block 3...\nException: No available Buffers" To_test.test3

let all_tests () =
  [
    Alcotest.test_case "Test 1" `Quick test1;
    Alcotest.test_case "Test 2" `Quick test2;
    Alcotest.test_case "Test 3" `Quick test3;
  ]
