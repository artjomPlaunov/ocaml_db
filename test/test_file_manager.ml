module To_test = struct
  (*********************************)
  (*      file manager tests       *)
  (*********************************)
  let test1 =
    let file_mgr =
      File.File_manager.make ~db_dirname:"db_test1" ~block_size:400
    in
    let block = File.Block_id.make ~filename:"testfile" ~block_num:100 in
    let page1 = File.Page.make (File.File_manager.get_blocksize file_mgr) in
    let pos1 = 88 in
    let _ = File.Page.set_string page1 pos1 "abcdefghijklm" in
    let size = File.Page.max_len (String.length "abcdefghijklm") in
    let pos2 = pos1 + size in
    let _ = File.Page.set_int32 page1 pos2 (Int32.of_int 69) in
    let _ = File.File_manager.write file_mgr block page1 in
    let page2 = File.Page.make (File.File_manager.get_blocksize file_mgr) in
    let _ = File.File_manager.read file_mgr block page2 in
    let s1 =
      Printf.sprintf "offset %d contains %d," pos2
        (Int32.to_int (File.Page.get_int32 page2 pos2))
    in
    let s2 =
      Printf.sprintf "offset %d constains %s" pos1
        (File.Page.get_string page2 pos1)
    in
    s1 ^ s2

  let test2 =
    let file_mgr =
      File.File_manager.make ~db_dirname:"db_test2" ~block_size:4096
    in
    let blocksize = File.File_manager.get_blocksize file_mgr in
    let block = File.Block_id.make ~filename:"tbl1" ~block_num:2 in
    let page1 = File.Page.make blocksize in
    let pos1 = 4091 in
    let _ = File.Page.set_string page1 pos1 "a" in
    let _ = File.File_manager.write file_mgr block page1 in
    let page2 = File.Page.make blocksize in
    let _ = File.File_manager.read file_mgr block page2 in
    Printf.sprintf "offset %d contains %s" pos1
      (File.Page.get_string page2 pos1)

  (*********************************)
  (*       log manager tests       *)
  (*********************************)
  let create_log_record str num =
    let num_pos = File.Page.max_len (String.length str) in
    let blocksize = num_pos + 4 in
    let page = File.Page.make blocksize in
    let _ = File.Page.set_string page 0 str in 
    let _ = File.Page.set_int32 page num_pos (Int32.of_int num) in 
    page

  let create_records log_manager start end_ =
    let _ = Printf.printf "Creating records: " in
    let _ =
      for i = start to end_ do
        let record = create_log_record ("record" ^ string_of_int i) (i) in
        let lsn = Log_manager.append log_manager (File.Page.contents record) in
        Printf.printf "%d " lsn
      done
    in
    Printf.printf "\n"

  let print_log_records log_mgr msg =
    let _ = Printf.printf "%s\n" msg in
    let iter = Log_manager.get_iterator log_mgr in 
    let rec iterate iter = 
      if Log_manager__Log_iterator.has_next iter 
      then 

        let b = Log_manager__Log_iterator.next iter in 
        let page = File.Page.from_bytes b in 
        let s = File.Page.get_string page 0 in 
        let num_pos = File.Page.max_len (String.length s) in 
        let v = Int32.to_int (File.Page.get_int32 page num_pos) in 
        let _ = Printf.printf "[%s, %d]" s v in 
        iterate iter
      else 
        () 
      in 
    iterate iter       

  let test3 =
    let file_manager =
      File.File_manager.make ~db_dirname:"db_test3" ~block_size:512
    in
    let log_file = "log_test3" in
    let log_manager = Log_manager.make ~file_manager ~log_file in
    let _ = create_records log_manager 1 50 in 
    Log_manager.flush log_manager 50;
    let _ = print_log_records log_manager "The log file now has these records:" in
    "hello"
end

let test1 () =
  Alcotest.(check string)
    "same string" "offset 105 contains 69,offset 88 constains abcdefghijklm"
    To_test.test1

let test2 () =
  Alcotest.(check string) "same string" "offset 4091 contains a" To_test.test2

let test3 () = Alcotest.(check string) "same string" "hello" To_test.test3

let () =
  let open Alcotest in
  run "AllTests"
    [
      ( "File_manager",
        [ test_case "Test 1" `Quick test1; test_case "Test 2" `Quick test2 ] );
      ("Log_manager", [ test_case "Test 3" `Quick test3 ]);
    ]

