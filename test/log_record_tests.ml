module To_test = struct
  open File

  let test_create_int_log () =
    let file_manager =
      File_manager.make ~db_dirname:"db_test_create_int_log" ~block_size:1024
    in
    let log_file_name = "int_logs" in
    let log_manager = Log_manager.make ~file_manager ~log_file:log_file_name in
    let block = Block_id.make ~filename:"fname" ~block_num:1 in
    let n =
      Log_record.write_update_int_log_record log_manager 15 block 255
        (Int32.of_int 15)
    in
    let _ = Log_manager.flush log_manager 1 in
    let iter = Log_manager.get_iterator log_manager in
    let next_rec = Log_manager__Log_iterator.next iter in
    let int_rec = Log_record.make ~bytes:next_rec in
    Printf.sprintf "%s" (Log_record.to_string int_rec)

  let test_create_logs1 () =
    let file_manager =
      File_manager.make ~db_dirname:"db_test_create_logs" ~block_size:1024
    in
    let log_file_name = "logs" in
    let log_manager = Log_manager.make ~file_manager ~log_file:log_file_name in
    let blk = Block_id.make ~filename:"fname" ~block_num:1 in
    let n =
      Log_record.write_update_int_log_record log_manager 15 blk 255
        (Int32.of_int 15)
    in
    let n = Log_record.write_checkpoint_log_record log_manager in
    let _ = Log_manager.flush log_manager 1 in
    let iter = Log_manager.get_iterator log_manager in
    let next_rec = Log_manager__Log_iterator.next iter in
    let checkpoint_rec = Log_record.make ~bytes:next_rec in

    let next_rec = Log_manager__Log_iterator.next iter in

    let int_rec = Log_record.make ~bytes:next_rec in
    let s1 = Printf.sprintf "%s" (Log_record.to_string int_rec) in
    let s2 = Printf.sprintf "%s" (Log_record.to_string checkpoint_rec) in
    s1 ^ s2

  let test_create_logs2 () =
    let file_manager =
      File_manager.make ~db_dirname:"db_test_create_logs2" ~block_size:52
    in
    let log_file_name = "logs" in
    let log_manager = Log_manager.make ~file_manager ~log_file:log_file_name in
    let blk = Block_id.make ~filename:"fname" ~block_num:1 in
    for i = 0 to 2 do
      let blk = Block_id.make ~filename:"fname" ~block_num:i in
      let _ =
        Log_record.write_update_int_log_record log_manager i blk i
          (Int32.of_int (100 + i))
      in
      ()
    done;
    for i = 0 to 2 do
      let blk = Block_id.make ~filename:"fname" ~block_num:i in
      let _ =
        Log_record.write_update_string_log_record log_manager i blk i
          ("string value" ^ Printf.sprintf "%d" i)
      in
      ()
    done;
    for i = 0 to 2 do
      let _ = Log_record.write_checkpoint_log_record log_manager in
      ()
    done;
    for i = 0 to 2 do
      let _ = Log_record.write_commit_log_record log_manager 3 in
      ()
    done;
    for i = 0 to 2 do
      let _ = Log_record.write_rollback_log_record log_manager 4 in
      ()
    done;
    for i = 0 to 2 do
      let _ = Log_record.write_start_log_record log_manager 6 in
      ()
    done;
    let iterator = Log_manager.get_iterator log_manager in
    let s = "" in
    let rec iterate_records iterator s1 =
      if Log_manager__Log_iterator.has_next iterator then
        let bytes = Log_manager__Log_iterator.next iterator in
        let next_rec = Log_record.make ~bytes in
        let s2 = Printf.sprintf "%s" (Log_record.to_string next_rec) in
        iterate_records iterator s1 ^ s2
      else s1
    in
    iterate_records iterator s
end

let test_create_logs2_expected_string =
  "<UPDATE INT 0 fname, 0 0 100><UPDATE INT 1 fname, 1 1 101><UPDATE INT 2 \
   fname, 2 2 102><UPDATE INT 3 fname, 3 3 103><UPDATE INT 4 fname, 4 4 \
   104><UPDATE INT 5 fname, 5 5 105><UPDATE INT 6 fname, 6 6 106><UPDATE INT 7 \
   fname, 7 7 107><UPDATE INT 8 fname, 8 8 108><UPDATE INT 9 fname, 9 9 \
   109><UPDATE INT 10 fname, 10 10 110><UPDATE STRING 0 fname, 0 0 string \
   value0><UPDATE STRING 1 fname, 1 1 string value1><UPDATE STRING 2 fname, 2 \
   2 string value2><UPDATE STRING 3 fname, 3 3 string value3><UPDATE STRING 4 \
   fname, 4 4 string value4><UPDATE STRING 5 fname, 5 5 string value5><UPDATE \
   STRING 6 fname, 6 6 string value6><UPDATE STRING 7 fname, 7 7 string \
   value7><UPDATE STRING 8 fname, 8 8 string value8><UPDATE STRING 9 fname, 9 \
   9 string value9><UPDATE STRING 10 fname, 10 10 string \
   value10><CHECKPOINT><CHECKPOINT><CHECKPOINT><CHECKPOINT><CHECKPOINT><CHECKPOINT><CHECKPOINT><CHECKPOINT><CHECKPOINT><CHECKPOINT><CHECKPOINT><COMMIT \
   3><COMMIT 3><COMMIT 3><COMMIT 3><COMMIT 3><COMMIT 3><COMMIT 3><COMMIT \
   3><COMMIT 3><COMMIT 3><COMMIT 3><ROLLBACK 4><ROLLBACK 4><ROLLBACK \
   4><ROLLBACK 4><ROLLBACK 4><ROLLBACK 4><ROLLBACK 4><ROLLBACK 4><ROLLBACK \
   4><ROLLBACK 4><ROLLBACK 4><START 6><START 6><START 6><START 6><START \
   6><START 6><START 6><START 6><START 6><START 6><START 6>"

let test_create_int_log () =
  Alcotest.(check string)
    "same string" "<UPDATE INT 15 fname, 1 255 15>"
    (To_test.test_create_int_log ())

let test_create_logs1 () =
  Alcotest.(check string)
    "same string" "<UPDATE INT 15 fname, 1 255 15><CHECKPOINT>"
    (To_test.test_create_logs1 ())

let test_create_logs2 () =
  Alcotest.(check string)
    "same string" test_create_logs2_expected_string
    (To_test.test_create_logs2 ())

let all_tests () =
  [
    Alcotest.test_case "create int logs" `Quick test_create_int_log;
    Alcotest.test_case "create logs1" `Quick test_create_logs1;
    Alcotest.test_case "create logs2" `Slow test_create_logs2;
  ]
