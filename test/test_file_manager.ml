module To_test = struct
  let test1 = 
    let f_mgr = File.FileManager.make "db_test" 400 in
    let block = File.BlockId.make "testfile" 0 in 
    let p1 = File.Page.make (File.FileManager.get_blocksize f_mgr) in 
    let pos1 = 88 in
    let _ = File.Page.set_string p1 pos1 "abcdefghijklm" in 
    let size = File.Page.max_len (String.length "abcdefghijklm") in 
    let pos2 = pos1 + size in 
    let _ = File.Page.set_int32 p1 pos2 (Int32.of_int 69) in 
    let _ = File.FileManager.write f_mgr block p1 in 
    let p2 = File.Page.make (File.FileManager.get_blocksize f_mgr) in 
    let _ = File.FileManager.read f_mgr block p2 in 
    let s1 = Printf.sprintf "offset %d contains %d," pos2 (Int32.to_int (File.Page.get_int32 p2 pos2)) in 
    let s2 = Printf.sprintf "offset %d constains %s" pos1 (File.Page.get_string p2 pos1) in 
    s1 ^ s2

  let test2 = 
    let f_mgr = File.FileManager.make "db" 4096 in 
    let blocksize = File.FileManager.get_blocksize f_mgr in
    let block = File.BlockId.make "tbl1" 2 in 
    let page1 = File.Page.make blocksize in 
    let pos1 = 4091 in
    let _ = File.Page.set_string page1 pos1 "a" in 
    let _ = File.FileManager.write f_mgr block page1 in 
    let page2 = File.Page.make blocksize in 
    let _ = File.FileManager.read f_mgr block page2 in 
    Printf.sprintf "offset %d contains %s" pos1 (File.Page.get_string page2 pos1)
  end


let test1 () = 
  Alcotest.(check string) "same string" "offset 105 contains 69,offset 88 constains abcdefghijklm" (To_test.test1) 
  
let test2 () = 
  Alcotest.(check string) "same string" "offset 4091 contains a" (To_test.test2)

let () = 
  let open Alcotest in 
  run "FileManager" [
    "simple tests", [
      test_case "Test 1" `Quick test1;
      test_case "Test 2" `Quick test2;
    ];
  ]