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
    for i = 0 to 10 do
      let blk = Block_id.make ~filename:"fname" ~block_num:i in
      let _ =
        Log_record.write_update_int_log_record log_manager i blk i
          (Int32.of_int (100 + i))
      in
      ()
    done;
    for i = 0 to 100 do
      let blk = Block_id.make ~filename:"fname" ~block_num:i in
      let _ =
        Log_record.write_update_string_log_record log_manager i blk i
          ("string value" ^ Printf.sprintf "%d" i)
      in
      ()
    done;
    for i = 0 to 100 do
      let _ = Log_record.write_checkpoint_log_record log_manager in
      ()
    done;
    for i = 0 to 100 do
      let _ = Log_record.write_commit_log_record log_manager 3 in
      ()
    done;
    for i = 0 to 100 do
      let _ = Log_record.write_rollback_log_record log_manager 4 in
      ()
    done;
    for i = 0 to 100 do
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
   9 string value9><UPDATE STRING 10 fname, 10 10 string value10><UPDATE \
   STRING 11 fname, 11 11 string value11><UPDATE STRING 12 fname, 12 12 string \
   value12><UPDATE STRING 13 fname, 13 13 string value13><UPDATE STRING 14 \
   fname, 14 14 string value14><UPDATE STRING 15 fname, 15 15 string \
   value15><UPDATE STRING 16 fname, 16 16 string value16><UPDATE STRING 17 \
   fname, 17 17 string value17><UPDATE STRING 18 fname, 18 18 string \
   value18><UPDATE STRING 19 fname, 19 19 string value19><UPDATE STRING 20 \
   fname, 20 20 string value20><UPDATE STRING 21 fname, 21 21 string \
   value21><UPDATE STRING 22 fname, 22 22 string value22><UPDATE STRING 23 \
   fname, 23 23 string value23><UPDATE STRING 24 fname, 24 24 string \
   value24><UPDATE STRING 25 fname, 25 25 string value25><UPDATE STRING 26 \
   fname, 26 26 string value26><UPDATE STRING 27 fname, 27 27 string \
   value27><UPDATE STRING 28 fname, 28 28 string value28><UPDATE STRING 29 \
   fname, 29 29 string value29><UPDATE STRING 30 fname, 30 30 string \
   value30><UPDATE STRING 31 fname, 31 31 string value31><UPDATE STRING 32 \
   fname, 32 32 string value32><UPDATE STRING 33 fname, 33 33 string \
   value33><UPDATE STRING 34 fname, 34 34 string value34><UPDATE STRING 35 \
   fname, 35 35 string value35><UPDATE STRING 36 fname, 36 36 string \
   value36><UPDATE STRING 37 fname, 37 37 string value37><UPDATE STRING 38 \
   fname, 38 38 string value38><UPDATE STRING 39 fname, 39 39 string \
   value39><UPDATE STRING 40 fname, 40 40 string value40><UPDATE STRING 41 \
   fname, 41 41 string value41><UPDATE STRING 42 fname, 42 42 string \
   value42><UPDATE STRING 43 fname, 43 43 string value43><UPDATE STRING 44 \
   fname, 44 44 string value44><UPDATE STRING 45 fname, 45 45 string \
   value45><UPDATE STRING 46 fname, 46 46 string value46><UPDATE STRING 47 \
   fname, 47 47 string value47><UPDATE STRING 48 fname, 48 48 string \
   value48><UPDATE STRING 49 fname, 49 49 string value49><UPDATE STRING 50 \
   fname, 50 50 string value50><UPDATE STRING 51 fname, 51 51 string \
   value51><UPDATE STRING 52 fname, 52 52 string value52><UPDATE STRING 53 \
   fname, 53 53 string value53><UPDATE STRING 54 fname, 54 54 string \
   value54><UPDATE STRING 55 fname, 55 55 string value55><UPDATE STRING 56 \
   fname, 56 56 string value56><UPDATE STRING 57 fname, 57 57 string \
   value57><UPDATE STRING 58 fname, 58 58 string value58><UPDATE STRING 59 \
   fname, 59 59 string value59><UPDATE STRING 60 fname, 60 60 string \
   value60><UPDATE STRING 61 fname, 61 61 string value61><UPDATE STRING 62 \
   fname, 62 62 string value62><UPDATE STRING 63 fname, 63 63 string \
   value63><UPDATE STRING 64 fname, 64 64 string value64><UPDATE STRING 65 \
   fname, 65 65 string value65><UPDATE STRING 66 fname, 66 66 string \
   value66><UPDATE STRING 67 fname, 67 67 string value67><UPDATE STRING 68 \
   fname, 68 68 string value68><UPDATE STRING 69 fname, 69 69 string \
   value69><UPDATE STRING 70 fname, 70 70 string value70><UPDATE STRING 71 \
   fname, 71 71 string value71><UPDATE STRING 72 fname, 72 72 string \
   value72><UPDATE STRING 73 fname, 73 73 string value73><UPDATE STRING 74 \
   fname, 74 74 string value74><UPDATE STRING 75 fname, 75 75 string \
   value75><UPDATE STRING 76 fname, 76 76 string value76><UPDATE STRING 77 \
   fname, 77 77 string value77><UPDATE STRING 78 fname, 78 78 string \
   value78><UPDATE STRING 79 fname, 79 79 string value79><UPDATE STRING 80 \
   fname, 80 80 string value80><UPDATE STRING 81 fname, 81 81 string \
   value81><UPDATE STRING 82 fname, 82 82 string value82><UPDATE STRING 83 \
   fname, 83 83 string value83><UPDATE STRING 84 fname, 84 84 string \
   value84><UPDATE STRING 85 fname, 85 85 string value85><UPDATE STRING 86 \
   fname, 86 86 string value86><UPDATE STRING 87 fname, 87 87 string \
   value87><UPDATE STRING 88 fname, 88 88 string value88><UPDATE STRING 89 \
   fname, 89 89 string value89><UPDATE STRING 90 fname, 90 90 string \
   value90><UPDATE STRING 91 fname, 91 91 string value91><UPDATE STRING 92 \
   fname, 92 92 string value92><UPDATE STRING 93 fname, 93 93 string \
   value93><UPDATE STRING 94 fname, 94 94 string value94><UPDATE STRING 95 \
   fname, 95 95 string value95><UPDATE STRING 96 fname, 96 96 string \
   value96><UPDATE STRING 97 fname, 97 97 string value97><UPDATE STRING 98 \
   fname, 98 98 string value98><UPDATE STRING 99 fname, 99 99 string \
   value99><UPDATE STRING 100 fname, 100 100 string \
   value100><CHECKPOINT><CHECKPOINT><CHECKPOINT><CHECKPOINT><CHECKPOINT><CHECKPOINT><CHECKPOINT><CHECKPOINT><CHECKPOINT><CHECKPOINT><CHECKPOINT><CHECKPOINT><CHECKPOINT><CHECKPOINT><CHECKPOINT><CHECKPOINT><CHECKPOINT><CHECKPOINT><CHECKPOINT><CHECKPOINT><CHECKPOINT><CHECKPOINT><CHECKPOINT><CHECKPOINT><CHECKPOINT><CHECKPOINT><CHECKPOINT><CHECKPOINT><CHECKPOINT><CHECKPOINT><CHECKPOINT><CHECKPOINT><CHECKPOINT><CHECKPOINT><CHECKPOINT><CHECKPOINT><CHECKPOINT><CHECKPOINT><CHECKPOINT><CHECKPOINT><CHECKPOINT><CHECKPOINT><CHECKPOINT><CHECKPOINT><CHECKPOINT><CHECKPOINT><CHECKPOINT><CHECKPOINT><CHECKPOINT><CHECKPOINT><CHECKPOINT><CHECKPOINT><CHECKPOINT><CHECKPOINT><CHECKPOINT><CHECKPOINT><CHECKPOINT><CHECKPOINT><CHECKPOINT><CHECKPOINT><CHECKPOINT><CHECKPOINT><CHECKPOINT><CHECKPOINT><CHECKPOINT><CHECKPOINT><CHECKPOINT><CHECKPOINT><CHECKPOINT><CHECKPOINT><CHECKPOINT><CHECKPOINT><CHECKPOINT><CHECKPOINT><CHECKPOINT><CHECKPOINT><CHECKPOINT><CHECKPOINT><CHECKPOINT><CHECKPOINT><CHECKPOINT><CHECKPOINT><CHECKPOINT><CHECKPOINT><CHECKPOINT><CHECKPOINT><CHECKPOINT><CHECKPOINT><CHECKPOINT><CHECKPOINT><CHECKPOINT><CHECKPOINT><CHECKPOINT><CHECKPOINT><CHECKPOINT><CHECKPOINT><CHECKPOINT><CHECKPOINT><CHECKPOINT><CHECKPOINT><CHECKPOINT><COMMIT \
   3><COMMIT 3><COMMIT 3><COMMIT 3><COMMIT 3><COMMIT 3><COMMIT 3><COMMIT \
   3><COMMIT 3><COMMIT 3><COMMIT 3><COMMIT 3><COMMIT 3><COMMIT 3><COMMIT \
   3><COMMIT 3><COMMIT 3><COMMIT 3><COMMIT 3><COMMIT 3><COMMIT 3><COMMIT \
   3><COMMIT 3><COMMIT 3><COMMIT 3><COMMIT 3><COMMIT 3><COMMIT 3><COMMIT \
   3><COMMIT 3><COMMIT 3><COMMIT 3><COMMIT 3><COMMIT 3><COMMIT 3><COMMIT \
   3><COMMIT 3><COMMIT 3><COMMIT 3><COMMIT 3><COMMIT 3><COMMIT 3><COMMIT \
   3><COMMIT 3><COMMIT 3><COMMIT 3><COMMIT 3><COMMIT 3><COMMIT 3><COMMIT \
   3><COMMIT 3><COMMIT 3><COMMIT 3><COMMIT 3><COMMIT 3><COMMIT 3><COMMIT \
   3><COMMIT 3><COMMIT 3><COMMIT 3><COMMIT 3><COMMIT 3><COMMIT 3><COMMIT \
   3><COMMIT 3><COMMIT 3><COMMIT 3><COMMIT 3><COMMIT 3><COMMIT 3><COMMIT \
   3><COMMIT 3><COMMIT 3><COMMIT 3><COMMIT 3><COMMIT 3><COMMIT 3><COMMIT \
   3><COMMIT 3><COMMIT 3><COMMIT 3><COMMIT 3><COMMIT 3><COMMIT 3><COMMIT \
   3><COMMIT 3><COMMIT 3><COMMIT 3><COMMIT 3><COMMIT 3><COMMIT 3><COMMIT \
   3><COMMIT 3><COMMIT 3><COMMIT 3><COMMIT 3><COMMIT 3><COMMIT 3><COMMIT \
   3><COMMIT 3><COMMIT 3><ROLLBACK 4><ROLLBACK 4><ROLLBACK 4><ROLLBACK \
   4><ROLLBACK 4><ROLLBACK 4><ROLLBACK 4><ROLLBACK 4><ROLLBACK 4><ROLLBACK \
   4><ROLLBACK 4><ROLLBACK 4><ROLLBACK 4><ROLLBACK 4><ROLLBACK 4><ROLLBACK \
   4><ROLLBACK 4><ROLLBACK 4><ROLLBACK 4><ROLLBACK 4><ROLLBACK 4><ROLLBACK \
   4><ROLLBACK 4><ROLLBACK 4><ROLLBACK 4><ROLLBACK 4><ROLLBACK 4><ROLLBACK \
   4><ROLLBACK 4><ROLLBACK 4><ROLLBACK 4><ROLLBACK 4><ROLLBACK 4><ROLLBACK \
   4><ROLLBACK 4><ROLLBACK 4><ROLLBACK 4><ROLLBACK 4><ROLLBACK 4><ROLLBACK \
   4><ROLLBACK 4><ROLLBACK 4><ROLLBACK 4><ROLLBACK 4><ROLLBACK 4><ROLLBACK \
   4><ROLLBACK 4><ROLLBACK 4><ROLLBACK 4><ROLLBACK 4><ROLLBACK 4><ROLLBACK \
   4><ROLLBACK 4><ROLLBACK 4><ROLLBACK 4><ROLLBACK 4><ROLLBACK 4><ROLLBACK \
   4><ROLLBACK 4><ROLLBACK 4><ROLLBACK 4><ROLLBACK 4><ROLLBACK 4><ROLLBACK \
   4><ROLLBACK 4><ROLLBACK 4><ROLLBACK 4><ROLLBACK 4><ROLLBACK 4><ROLLBACK \
   4><ROLLBACK 4><ROLLBACK 4><ROLLBACK 4><ROLLBACK 4><ROLLBACK 4><ROLLBACK \
   4><ROLLBACK 4><ROLLBACK 4><ROLLBACK 4><ROLLBACK 4><ROLLBACK 4><ROLLBACK \
   4><ROLLBACK 4><ROLLBACK 4><ROLLBACK 4><ROLLBACK 4><ROLLBACK 4><ROLLBACK \
   4><ROLLBACK 4><ROLLBACK 4><ROLLBACK 4><ROLLBACK 4><ROLLBACK 4><ROLLBACK \
   4><ROLLBACK 4><ROLLBACK 4><ROLLBACK 4><ROLLBACK 4><ROLLBACK 4><ROLLBACK \
   4><ROLLBACK 4><START 6><START 6><START 6><START 6><START 6><START 6><START \
   6><START 6><START 6><START 6><START 6><START 6><START 6><START 6><START \
   6><START 6><START 6><START 6><START 6><START 6><START 6><START 6><START \
   6><START 6><START 6><START 6><START 6><START 6><START 6><START 6><START \
   6><START 6><START 6><START 6><START 6><START 6><START 6><START 6><START \
   6><START 6><START 6><START 6><START 6><START 6><START 6><START 6><START \
   6><START 6><START 6><START 6><START 6><START 6><START 6><START 6><START \
   6><START 6><START 6><START 6><START 6><START 6><START 6><START 6><START \
   6><START 6><START 6><START 6><START 6><START 6><START 6><START 6><START \
   6><START 6><START 6><START 6><START 6><START 6><START 6><START 6><START \
   6><START 6><START 6><START 6><START 6><START 6><START 6><START 6><START \
   6><START 6><START 6><START 6><START 6><START 6><START 6><START 6><START \
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
    Alcotest.test_case "create logs2" `Quick test_create_logs2;
  ]
