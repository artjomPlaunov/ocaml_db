module To_test = struct
  open File
  open Storage_manager

  let write_output_to_file output =
    let out_file = Filename.concat "tmp_storage_test" "test_output.txt" in
    let oc = open_out out_file in
    output_string oc output;
    close_out oc

  let print_storage_contents sm =
    let fm = sm.file_manager in
    let block_size = File_manager.get_blocksize fm in
    let sfile = sm.storage_file in
    let size = File_manager.size fm sfile in
    let output = Buffer.create 256 in
    Buffer.add_string output "Storage contents:\n";
    for i = 0 to size - 1 do
      let block = Block_id.make ~filename:sfile ~block_num:i in
      let page = Page.make ~block_size in
      File_manager.read fm block page;
      let value = Int32.to_int (Page.get_int32 page 0) in
      Buffer.add_string output (Printf.sprintf "Block %d: %d\n" i value)
    done;
    Buffer.contents output

  let print_free_list sm =
    let fm = sm.file_manager in
    let head_page = sm.head_page in
    let sfile = sm.storage_file in
    let output = Buffer.create 256 in
    Buffer.add_string output "Free list:\n";
    let rec print_list ptr =
      if ptr = 0 then
        Buffer.add_string output "End of free list\n"
      else (
        Buffer.add_string output (Printf.sprintf "Block %d -> " ptr);
        let block = Block_id.make ~filename:sfile ~block_num:ptr in
        let page = Page.make ~block_size:(File_manager.get_blocksize fm) in
        File_manager.read fm block page;
        let next_ptr = Int32.to_int (Page.get_int32 page 0) in
        print_list next_ptr)
    in
    let start_ptr = Int32.to_int (Page.get_int32 head_page 0) in
    print_list start_ptr;
    Buffer.contents output

  let test_storage_manager () =
    let file_manager = File_manager.make ~db_dirname:"tmp_storage_test" ~block_size:8 in
    let storage_manager = Storage_manager.make ~file_manager ~storage_file:"test_storage" in
    let output = Buffer.create 256 in

    Buffer.add_string output "Initial state:\n";
    Buffer.add_string output (print_storage_contents storage_manager);
    Buffer.add_string output (print_free_list storage_manager);

    (* Create some pages with integers *)
    let pages = List.init 5 (fun i ->
      let page = Page.make ~block_size:8 in
      Page.set_int32 page 0 (Int32.of_int (i + 1));
      page)
    in

    Buffer.add_string output "\nInitial append of 5 blocks:\n";
    let blocks = List.map (fun page -> Storage_manager.append ~storage_manager ~page) pages in
    Buffer.add_string output (print_storage_contents storage_manager);
    Buffer.add_string output (print_free_list storage_manager);

    Buffer.add_string output "\nDeleting blocks 2 and 4:\n";
    Storage_manager.delete ~storage_manager ~block:(List.nth blocks 1);
    Storage_manager.delete ~storage_manager ~block:(List.nth blocks 3);
    Buffer.add_string output (print_storage_contents storage_manager);
    Buffer.add_string output (print_free_list storage_manager);

    Buffer.add_string output "\nDeleting remaining blocks (1, 3, and 5):\n";
    Storage_manager.delete ~storage_manager ~block:(List.nth blocks 0);
    Storage_manager.delete ~storage_manager ~block:(List.nth blocks 2);
    Storage_manager.delete ~storage_manager ~block:(List.nth blocks 4);
    Buffer.add_string output (print_storage_contents storage_manager);
    Buffer.add_string output (print_free_list storage_manager);

    Buffer.add_string output "\nAppending two new blocks:\n";
    let page1 = Page.make ~block_size:8 in
    let page2 = Page.make ~block_size:8 in
    Page.set_int32 page1 0 (Int32.of_int 200);
    Page.set_int32 page2 0 (Int32.of_int 201);
    let _ = Storage_manager.append ~storage_manager ~page:page1 in
    Buffer.add_string output "\nAfter first append:\n";
    Buffer.add_string output (print_storage_contents storage_manager);
    Buffer.add_string output (print_free_list storage_manager);
    
    let _ = Storage_manager.append ~storage_manager ~page:page2 in
    Buffer.add_string output "\nAfter second append:\n";
    Buffer.add_string output (print_storage_contents storage_manager);
    Buffer.add_string output (print_free_list storage_manager);
    
    Buffer.add_string output "\nUpdating block 3 with value 999:\n";
    let update_page = Page.make ~block_size:8 in
    Page.set_int32 update_page 0 (Int32.of_int 999);
    let block2 = Block_id.make ~filename:"test_storage" ~block_num:3 in
    Storage_manager.update ~storage_manager ~block:block2 ~page:update_page;
    Buffer.add_string output (print_storage_contents storage_manager);
    Buffer.add_string output (print_free_list storage_manager);

    let final_output = Buffer.contents output in
    write_output_to_file final_output;
    final_output
end

let expected_output = "Initial state:\nStorage contents:\nBlock 0: 0\nFree list:\nEnd of free list\n\nInitial append of 5 blocks:\nStorage contents:\nBlock 0: 0\nBlock 1: 1\nBlock 2: 2\nBlock 3: 3\nBlock 4: 4\nBlock 5: 5\nFree list:\nEnd of free list\n\nDeleting blocks 2 and 4:\nStorage contents:\nBlock 0: 4\nBlock 1: 1\nBlock 2: 0\nBlock 3: 3\nBlock 4: 2\nBlock 5: 5\nFree list:\nBlock 4 -> Block 2 -> End of free list\n\nDeleting remaining blocks (1, 3, and 5):\nStorage contents:\nBlock 0: 5\nBlock 1: 4\nBlock 2: 0\nBlock 3: 1\nBlock 4: 2\nBlock 5: 3\nFree list:\nBlock 5 -> Block 3 -> Block 1 -> Block 4 -> Block 2 -> End of free list\n\nAppending two new blocks:\n\nAfter first append:\nStorage contents:\nBlock 0: 3\nBlock 1: 4\nBlock 2: 0\nBlock 3: 1\nBlock 4: 2\nBlock 5: 200\nFree list:\nBlock 3 -> Block 1 -> Block 4 -> Block 2 -> End of free list\n\nAfter second append:\nStorage contents:\nBlock 0: 1\nBlock 1: 4\nBlock 2: 0\nBlock 3: 201\nBlock 4: 2\nBlock 5: 200\nFree list:\nBlock 1 -> Block 4 -> Block 2 -> End of free list\n\nUpdating block 3 with value 999:\nStorage contents:\nBlock 0: 1\nBlock 1: 4\nBlock 2: 0\nBlock 3: 999\nBlock 4: 2\nBlock 5: 200\nFree list:\nBlock 1 -> Block 4 -> Block 2 -> End of free list\n"

let test_storage_manager () =
  Alcotest.(check string)
    "storage manager operations"
    expected_output
    (To_test.test_storage_manager ())

let all_tests () =
  [Alcotest.test_case "storage manager test" `Quick test_storage_manager] 