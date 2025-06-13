module To_test = struct
  open File_manager

  let write_and_read_string_and_int32 () =
    let file_mgr =
      File_manager.make ~db_dirname:"db_test_write_read_string_int32"
        ~block_size:400
    in
    let block_size = File_manager.get_blocksize file_mgr in
    (* write a string "abcdefghijklm" and int 69 into block starting at position 88 *)
    let block = Block_id.make ~filename:"file1" ~block_num:100 in
    let page1 = Page.make ~block_size in
    let str_pos = 88 in
    Page.set_string page1 str_pos "abcdefghijklm";
    let size = Page.max_len (String.length "abcdefghijklm") in
    let num_pos = str_pos + size in
    Page.set_int32 page1 num_pos (Int32.of_int 69);
    File_manager.write file_mgr block page1;
    (* read the block back into a new page (page2) to ensure contents stay the same *)
    let page2 = Page.make ~block_size in
    File_manager.read file_mgr block page2;
    assert (page1 = page2);
    let str_offset_msg =
      Printf.sprintf "offset %d contains %s, " str_pos
        (Page.get_string page2 str_pos)
    in
    let int32_offset_msg =
      Printf.sprintf "offset %d contains %d" num_pos
        (Int32.to_int (Page.get_int32 page2 num_pos))
    in
    str_offset_msg ^ int32_offset_msg

  let write_and_read_last_byte () =
    let file_mgr =
      File_manager.make ~db_dirname:"db_test_write_read_last_byte"
        ~block_size:4096
    in
    let block_size = File_manager.get_blocksize file_mgr in
    (* Write to the last byte in a block *)
    let block = Block_id.make ~filename:"file2" ~block_num:2 in
    let page1 = Page.make ~block_size in
    let pos1 = 4091 in
    Page.set_string page1 pos1 "a";
    File_manager.write file_mgr block page1;
    let page2 = Page.make ~block_size in
    File_manager.read file_mgr block page2;
    Printf.sprintf "offset %d contains %s" pos1 (Page.get_string page2 pos1)
end

let test_write_and_read_string_and_int32 () =
  Alcotest.(check string)
    "verify string and int32 are correctly written and read back"
    "offset 88 contains abcdefghijklm, offset 105 contains 69"
    (To_test.write_and_read_string_and_int32 ())

let test_write_and_read_last_byte () =
  Alcotest.(check string)
    "verify last byte write and read" "offset 4091 contains a"
    (To_test.write_and_read_last_byte ())

let all_tests () =
  [
    Alcotest.test_case "write and read string and int32" `Quick
      test_write_and_read_string_and_int32;
    Alcotest.test_case "write and read last byte" `Quick
      test_write_and_read_last_byte;
  ]
