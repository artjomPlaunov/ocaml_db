let _blk = Block.make "testfile" 2 in
let f_mgr = FileManager.make "db_test" 1 in
let _ = FileManager.get_file f_mgr "test" in
()

