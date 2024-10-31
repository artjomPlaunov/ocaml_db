module To_test = struct
  open File
  open Test_utils

  let single_buffer_update () =
    let env = make_buffer_test_env ~db_name:"buffertest1" ~num_buffers:1 in
    let block1 = File.Block_id.make ~filename:"tmp_testfile" ~block_num:1 in
    let block2 = File.Block_id.make ~filename:"tmp_testfile" ~block_num:2 in
    let buf1 = Buffer_manager.pin env.buffer_manager block1 in
    let page = Buffer_manager__Db_buffer.contents buf1 in
    let n = Int32.to_int (File.Page.get_int32 page 80) in
    let before_msg = Printf.sprintf "Buffer contents before update: %d\n" n in
    Page.set_int32 page 80 (Int32.of_int (n + 1));
    let after_msg = Printf.sprintf "Buffer contents after update: %d\n" (n + 1) in
    Buffer_manager__Db_buffer.set_modified buf1 1 0;
    Buffer_manager.unpin env.buffer_manager buf1;
    let buf2 = Buffer_manager.pin env.buffer_manager block2 in
    Buffer_manager.unpin env.buffer_manager buf2;
    cleanup_buffer_test_env env;
    before_msg ^ after_msg

  let multiple_buffer_update () =
    let env = make_buffer_test_env ~db_name:"buffertest2" ~num_buffers:3 in
    let block1 = Block_id.make ~filename:"tmp_testfile" ~block_num:0 in
    let buf1 = Buffer_manager.pin env.buffer_manager block1 in
    let p = Buffer_manager__Db_buffer.contents buf1 in
    let n = Int32.to_int (File.Page.get_int32 p 0) in
    let before_msg1 = Printf.sprintf "Buffer contents at offset 0 before first update: %d\n" n in
    Page.set_int32 p 0 (Int32.of_int (n + 1));
    let after_msg1 = Printf.sprintf "Buffer contents at offset 0 after first update: %d\n" (n + 1) in
    Buffer_manager__Db_buffer.set_modified buf1 1 0;
    Buffer_manager.unpin env.buffer_manager buf1;
    
    let block2 = Block_id.make ~filename:"tmp_testfile" ~block_num:100 in
    let block3 = Block_id.make ~filename:"tmp_testfile" ~block_num:3 in
    let block4 = Block_id.make ~filename:"tmp_testfile" ~block_num:4 in
    let buf2 = Buffer_manager.pin env.buffer_manager block2 in
    let p2 = Buffer_manager__Db_buffer.contents buf2 in
    let _ = Int32.to_int (File.Page.get_int32 p2 0) in
    let buf3 = Buffer_manager.pin env.buffer_manager block3 in
    let buf4 = Buffer_manager.pin env.buffer_manager block4 in
    Buffer_manager.unpin env.buffer_manager buf2;
    Buffer_manager.unpin env.buffer_manager buf3;
    Buffer_manager.unpin env.buffer_manager buf4;

    let buf2 = Buffer_manager.pin env.buffer_manager block1 in
    let p2 = Buffer_manager__Db_buffer.contents buf2 in
    let n2 = Int32.to_int (File.Page.get_int32 p2 80) in
    let before_msg2 = Printf.sprintf "Buffer contents at offset 80 before second update: %d\n" n2 in
    Page.set_int32 p2 80 (Int32.of_int 9999);
    let after_msg2 = Printf.sprintf "Buffer contents at offset 80 after second update: %d\n" 9999 in
    Buffer_manager__Db_buffer.set_modified buf2 1 0;
    Buffer_manager.unpin env.buffer_manager buf2;
    cleanup_buffer_test_env env;
    before_msg1 ^ after_msg1 ^ before_msg2 ^ after_msg2

  let pin_unpin () =
    let env = make_buffer_test_env ~db_name:"buffertest3" ~num_buffers:3 in
    let block0 = Block_id.make ~filename:"tmp_testfile" ~block_num:0 in
    let block1 = Block_id.make ~filename:"tmp_testfile" ~block_num:1 in
    let block2 = Block_id.make ~filename:"tmp_testfile" ~block_num:2 in
    let block3 = Block_id.make ~filename:"tmp_testfile" ~block_num:3 in
    
    (* Pin blocks 0, 1, 2 *)
    let buf0 = Buffer_manager.pin env.buffer_manager block0 in
    let buf1 = Buffer_manager.pin env.buffer_manager block1 in
    let buf2 = Buffer_manager.pin env.buffer_manager block2 in
    
    (* Buffer is full. Unpin block 1. *)
    Buffer_manager.unpin env.buffer_manager buf1;
    
    (* Pin block 0 again (already pinned). *)
    let buf3 = Buffer_manager.pin env.buffer_manager block0 in
    
    (* Pin block 1 again. Buffer is now full again. *)
    let buf4 = Buffer_manager.pin env.buffer_manager block1 in
    
    let s1 = Printf.sprintf "Attempting to pin block 3...\n" in
    let s2 =
      try
        let _ = Buffer_manager.pin env.buffer_manager block3 in
        "test fail"
      with Buffer_manager.BufferAbortException ->
        Printf.sprintf "Exception: No available Buffers"
    in
    Buffer_manager.unpin env.buffer_manager buf2;
    (* Now we should be able to pin block3, after unpinning block2. *)
    let buf5 = Buffer_manager.pin env.buffer_manager block3 in
    cleanup_buffer_test_env env;
    s1 ^ s2

  let double_pin () =
    let env = make_buffer_test_env ~db_name:"buffertest4" ~num_buffers:1 in
    let block0 = Block_id.make ~filename:"tmp_testfile" ~block_num:0 in
    let block1 = Block_id.make ~filename:"tmp_testfile" ~block_num:1 in
    let buf0 = Buffer_manager.pin env.buffer_manager block0 in
    let buf0 = Buffer_manager.pin env.buffer_manager block0 in
    Buffer_manager.unpin env.buffer_manager buf0;
    
    let s1 = Printf.sprintf "Attempting to pin block 1..." in
    let s2 =
      try
        let _ = Buffer_manager.pin env.buffer_manager block1 in
        "test fail"
      with Buffer_manager.BufferAbortException ->
        Printf.sprintf "Exception: No available Buffers..."
    in
    Buffer_manager.unpin env.buffer_manager buf0;
    let buf1 = Buffer_manager.pin env.buffer_manager block1 in
    let s3 =
      try
        let _ = Buffer_manager.pin env.buffer_manager block1 in
        "Successfully pinned..."
      with Buffer_manager.BufferAbortException -> 
        Printf.sprintf "test fail"
    in
    cleanup_buffer_test_env env;
    s1 ^ s2 ^ s3
end

(* Expected outputs *)
let single_buffer_expected = "Buffer contents before update: 0\nBuffer contents after update: 1\n"
let multiple_buffer_expected = 
  "Buffer contents at offset 0 before first update: 0\n\
   Buffer contents at offset 0 after first update: 1\n\
   Buffer contents at offset 80 before second update: 0\n\
   Buffer contents at offset 80 after second update: 9999\n"
let pin_unpin_expected = 
  "Attempting to pin block 3...\nException: No available Buffers"
let double_pin_expected = 
  "Attempting to pin block 1...Exception: No available Buffers...Successfully pinned..."

(* Test drivers *)
let test_single_buffer_update () =
  Alcotest.(check string) 
    "single buffer update log" 
    single_buffer_expected 
    (To_test.single_buffer_update ())

let test_multiple_buffer_update () =
  Alcotest.(check string)
    "multiple buffer update log"
    multiple_buffer_expected
    (To_test.multiple_buffer_update ())

let test_pin_unpin () =
  Alcotest.(check string)
    "pin unpin message"
    pin_unpin_expected
    (To_test.pin_unpin ())

let test_double_pin () =
  Alcotest.(check string)
    "double pin message"
    double_pin_expected
    (To_test.double_pin ())

let all_tests () =
  [
    Alcotest.test_case "update single buffer" `Quick test_single_buffer_update;
    Alcotest.test_case "update multiple buffers" `Quick test_multiple_buffer_update;
    Alcotest.test_case "pin and unpin" `Quick test_pin_unpin;
    Alcotest.test_case "double pinning" `Quick test_double_pin;
  ]
