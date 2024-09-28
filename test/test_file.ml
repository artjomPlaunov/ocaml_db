let () = 
let f_mgr = File.FileManager.make "db_test" 400 in
let block = File.BlockId.make "testfile" 2 in 
let p1 = File.Page.make (File.FileManager.get_blocksize f_mgr) in 
let pos1 = 88 in
let _ = File.Page.set_string p1 pos1 "abcdefghijklm" in 
let size = File.Page.max_len (String.length "abcdefghijklm") in 
let pos2 = pos1 + size in 
let _ = File.Page.set_int32 p1 pos2 (Int32.of_int 345) in 
let _ = File.FileManager.write f_mgr block p1 in 
let p2 = File.Page.make (File.FileManager.get_blocksize f_mgr) in 
let _ = File.FileManager.read f_mgr block p2 in 
let _ = Printf.printf "offset %d contains %d\n" pos2 (Int32.to_int (File.Page.get_int32 p2 pos2)) in 
let _ = Printf.printf "offset %d constains %s\n" pos1 (File.Page.get_string p2 pos1) in 
Printf.printf "hello"