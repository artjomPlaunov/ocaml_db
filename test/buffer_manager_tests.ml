module To_test = struct

  let test1 = 
    let file_manager =
      File.File_manager.make ~db_dirname:"buffertest1" ~block_size:512
    in
    let log_file = "buffertest_log" in 
    let log_manager = Log_manager.make ~file_manager ~log_file in 
    let buffer_manager = Buffer_manager.make ~file_manager ~log_manager ~num_buffers:1 () in 
    let block1 = File.Block_id.make ~filename:"testfile" ~block_num:1 in 
    let block2 = File.Block_id.make ~filename:"testfile" ~block_num:2 in 
    let buf1 = Buffer_manager.pin buffer_manager block1 in 
    let page = Buffer_manager__Db_buffer.contents buf1 in 
    let n = Int32.to_int (File.Page.get_int32 page 80) in 
    let _ = File.Page.set_int32 page 80 (Int32.of_int (n+1)) in
    let _ = Buffer_manager__Db_buffer.set_modified buf1 1 0 in 
    let _ = Printf.printf "new value is %d\n" (n+1) in 
    let _ = Buffer_manager.unpin buffer_manager buf1 in 
    let buf2 = Buffer_manager.pin buffer_manager block2 in
    Test_utils.no_diff "buffertest1/testfile" "buffer_manager_output/test1.txt"

  let test2 = 
    let file_manager =
      File.File_manager.make ~db_dirname:"buffertest" ~block_size:512
    in
    let log_file = "buffertest_log" in 
    let log_manager = Log_manager.make ~file_manager ~log_file in 
    let buffer_manager = Buffer_manager.make ~file_manager ~log_manager ~num_buffers:3 () in 
    let block = File.Block_id.make ~filename:"testfile" ~block_num:1 in 
    let buff = Buffer_manager.pin buffer_manager block in 
    let p = Buffer_manager__Db_buffer.contents buff in 
    let n = Int32.to_int (File.Page.get_int32 p 80) in 
    let _ = File.Page.set_int32 p 80 (Int32.of_int (n+1)) in
    let _ = Buffer_manager__Db_buffer.set_modified buff 1 0 in 
    let _ = Printf.printf "new value is %d\n" (n+1) in 
    let _ = Buffer_manager.unpin buffer_manager buff in 
    let block2 = File.Block_id.make ~filename:"testfile" ~block_num:2 in 
    let block3 = File.Block_id.make ~filename:"testfile" ~block_num:3 in 
    let block4 = File.Block_id.make ~filename:"testfile" ~block_num:4 in 
    let buf2 = Buffer_manager.pin buffer_manager block2 in 
    let buf3 = Buffer_manager.pin buffer_manager block3 in 
    let buf4 = Buffer_manager.pin buffer_manager block4 in 
    let _ = Buffer_manager.unpin buffer_manager buf2 in 
    let buf2 = Buffer_manager.pin buffer_manager block in 
    let p2 = Buffer_manager__Db_buffer.contents buf2 in
    let _ = File.Page.set_int32 p2 80 (Int32.of_int 9999) in 
    let _ = Buffer_manager__Db_buffer.set_modified buf2 1 0 in 
    let _ = Buffer_manager.unpin buffer_manager buf2 in  
    "hello"
end

let test1 () = Alcotest.(check bool) "bool equality" true To_test.test1

let test2 () = Alcotest.(check string) "same string" "hello" To_test.test2
