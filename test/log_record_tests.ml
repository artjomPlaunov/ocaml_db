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
    let int_rec = Log_record.make ~byte:next_rec in
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
    let int_rec = Log_record.make ~byte:next_rec in

    let next_rec = Log_manager__Log_iterator.next iter in 

    let checkpoint_rec = Log_record.make ~byte:next_rec in 
    let s1 = Printf.sprintf "%s" (Log_record.to_string int_rec) in 
    let s2 = Printf.sprintf "%s" (Log_record.to_string checkpoint_rec)in 
    s1 ^ s2
end

let test_create_int_log () =
  Alcotest.(check string) "same string" "<UPDATE INT 15 fname, 1 255 15>" (To_test.test_create_int_log ())
  let test_create_logs1 () =
    Alcotest.(check string) "same string" "<CHECKPOINT><UPDATE INT 15 fname, 1 255 15>" (To_test.test_create_logs1 ())


let all_tests () = [ 
  Alcotest.test_case "create int logs" `Quick test_create_int_log;
  Alcotest.test_case "create logs1" `Quick test_create_logs1 ]
