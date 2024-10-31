module To_test = struct
  open File

  let test_create_int_log () =
    let env = Test_utils.make_log_record_test_env ~db_name:"log_record_test1" in
    let block = Block_id.make ~filename:"fname" ~block_num:1 in
    let _ = Log_record.write_update_int_log_record env.log_manager 15 block 255 (Int32.of_int 15) in
    let _ = Log_manager.flush env.log_manager 1 in
    Test_utils.get_logs env.log_manager

  let test_create_checkpoint_and_int () =
    let env = Test_utils.make_log_record_test_env ~db_name:"log_record_test2" in
    let block = Block_id.make ~filename:"fname" ~block_num:1 in
    let _ = Log_record.write_update_int_log_record env.log_manager 15 block 255 (Int32.of_int 15) in
    let _ = Log_record.write_checkpoint_log_record env.log_manager in
    let _ = Log_manager.flush env.log_manager 1 in
    Test_utils.get_logs env.log_manager

  let test_create_multiple_records () =
    let env = Test_utils.make_log_record_test_env ~db_name:"log_record_test3" in
    
    (* Create int records *)
    for i = 0 to 2 do
      let block = Block_id.make ~filename:"fname" ~block_num:i in
      let _ = Log_record.write_update_int_log_record env.log_manager i block i (Int32.of_int (100 + i)) in
      ()
    done;

    (* Create string records *)  
    for i = 0 to 2 do
      let block = Block_id.make ~filename:"fname" ~block_num:i in
      let _ = Log_record.write_update_string_log_record env.log_manager i block i ("string value" ^ string_of_int i) in
      ()
    done;

    (* Create checkpoint records *)
    for i = 0 to 2 do
      let _ = Log_record.write_checkpoint_log_record env.log_manager in
      ()
    done;

    (* Create commit records *)
    for i = 0 to 2 do
      let _ = Log_record.write_commit_log_record env.log_manager 3 in
      ()
    done;

    (* Create rollback records *)
    for i = 0 to 2 do
      let _ = Log_record.write_rollback_log_record env.log_manager 4 in
      ()
    done;

    (* Create start records *)
    for i = 0 to 2 do
      let _ = Log_record.write_start_log_record env.log_manager 6 in
      ()
    done;

    Test_utils.get_logs env.log_manager
end

(* Expected test outputs *)
let int_log_expected = "<UPDATE INT 15 fname, 1 255 15>\n"

let checkpoint_and_int_expected = "<CHECKPOINT>\n<UPDATE INT 15 fname, 1 255 15>\n"

let multiple_records_expected = {|<START 6>
<START 6>
<START 6>
<ROLLBACK 4>
<ROLLBACK 4>
<ROLLBACK 4>
<COMMIT 3>
<COMMIT 3>
<COMMIT 3>
<CHECKPOINT>
<CHECKPOINT>
<CHECKPOINT>
<UPDATE STRING 2 fname, 2 2 string value2>
<UPDATE STRING 1 fname, 1 1 string value1>
<UPDATE STRING 0 fname, 0 0 string value0>
<UPDATE INT 2 fname, 2 2 102>
<UPDATE INT 1 fname, 1 1 101>
<UPDATE INT 0 fname, 0 0 100>
|}

(* Test drivers *)
let test_create_int_log () =
  Alcotest.(check string)
    "create int log record"
    int_log_expected
    (To_test.test_create_int_log ())

let test_create_checkpoint_and_int () =
  Alcotest.(check string)
    "create checkpoint and int records"
    checkpoint_and_int_expected
    (To_test.test_create_checkpoint_and_int ())

let test_create_multiple_records () =
  Alcotest.(check string)
    "create multiple record types"
    multiple_records_expected
    (To_test.test_create_multiple_records ())

let all_tests () =
  [
    Alcotest.test_case "create int log" `Quick test_create_int_log;
    Alcotest.test_case "create checkpoint and int" `Quick test_create_checkpoint_and_int;
    Alcotest.test_case "create multiple records" `Quick test_create_multiple_records;
  ]
