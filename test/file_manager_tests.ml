module To_test = struct
  open File

  let test1 =
    let file_mgr = File_manager.make ~db_dirname:"db_test1" ~block_size:400 in
    let block_size = File_manager.get_blocksize file_mgr in
    (* write a string "abcdefghijklm" and number 69 into block starting at position 88 *)
    let block = Block_id.make ~filename:"file1" ~block_num:100 in
    let page1 = Page.make ~block_size in
    let str_pos = 88 in
    Page.set_string page1 str_pos "abcdefghijklm";
    let size = File.Page.max_len (String.length "abcdefghijklm") in
    let num_pos = str_pos + size in
    Page.set_int32 page1 num_pos (Int32.of_int 69);
    File_manager.write file_mgr block page1;
    (* read block the block back into new page (page2) to make sure contents stay the same *)
    let page2 = Page.make ~block_size in
    File_manager.read file_mgr block page2;
    assert (page1 = page2);
    let s1 =
      Printf.sprintf "offset %d constains %s, " str_pos
        (Page.get_string page2 str_pos)
    in
    let s2 =
      Printf.sprintf "offset %d contains %d" num_pos
        (Int32.to_int (Page.get_int32 page2 num_pos))
    in
    s1 ^ s2

  let test2 =
    let file_mgr = File_manager.make ~db_dirname:"db_test2" ~block_size:4096 in
    let block_size = File_manager.get_blocksize file_mgr in
    let block = Block_id.make ~filename:"file2" ~block_num:2 in
    let page1 = Page.make ~block_size in
    let pos1 = 4091 in
    Page.set_string page1 pos1 "a";
    File_manager.write file_mgr block page1;
    let page2 = Page.make ~block_size in
    File_manager.read file_mgr block page2;
    Printf.sprintf "offset %d contains %s" pos1 (Page.get_string page2 pos1)
end

let test1 () =
  Alcotest.(check string)
    "same string" "offset 88 constains abcdefghijklm, offset 105 contains 69"
    To_test.test1

let test2 () =
  Alcotest.(check string) "same string" "offset 4091 contains a" To_test.test2

let all_test () =
  [
    Alcotest.test_case "Test 1" `Quick test1;
    Alcotest.test_case "Test 2" `Quick test2;
  ]
