module To_test = struct
  open File

  let test_create_int_log () =
    let file_manager =
      File_manager.make ~db_dirname:"db_test_create_int_logs" ~block_size:1024
    in
    let log_file_name = "int_logs" in
    let log_manager = Log_manager.make ~file_manager ~log_file:log_file_name in
    let blk = Block_id.make ~filename:"fname" ~block_num:1 in
    let n =
      Log_record.write_update_int_log_record log_manager 15 blk 255
        (Int32.of_int 15)
    in
    let _ = Log_manager.flush log_manager 1 in
    let iter = Log_manager.get_iterator log_manager in
    let next_rec = Log_manager__Log_iterator.next iter in
    let int_rec = Log_record.make ~byte:next_rec in
    Printf.sprintf "%s" (Log_record.to_string int_rec);
end

let test_create_int_log () =
  Alcotest.(check string) "same string" "<UPDATE INT 15 fname, 1 255 15>" (To_test.test_create_int_log ())

let all_tests () = [ Alcotest.test_case "create logs" `Quick test_create_int_log ]
