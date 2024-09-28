let () = 
let _blk = File.BlockId.make "testfile" 2 in
let f_mgr = File.FileManager.make "db_test" 1 in
let _ = File.FileManager.get_file f_mgr "test" in
Printf.printf "hello"