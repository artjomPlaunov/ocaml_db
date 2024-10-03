module To_test = struct
  open File

  let create_log_record str num =
    let num_pos = Page.max_len (String.length str) in
    let block_size = num_pos + 4 in
    let page = Page.make ~block_size in
    Page.set_string page 0 str;
    Page.set_int32 page num_pos (Int32.of_int num);
    page

  let create_records log_manager start end_ =
    Printf.printf "Creating records: ";
    for i = start to end_ do
      let record = create_log_record ("record" ^ string_of_int i) i in
      let lsn = Log_manager.append log_manager (File.Page.contents record) in
      Printf.printf "%d " lsn
    done;
    Printf.printf "\n"

  let print_log_records log_mgr msg =
    let _ = Printf.printf "%s\n" msg in
    let iter = Log_manager.get_iterator log_mgr in
    let rec iterate iter =
      if Log_manager__Log_iterator.has_next iter then (
        let b = Log_manager__Log_iterator.next iter in
        let page = Page.from_bytes b in
        let s = Page.get_string page 0 in
        let num_pos = Page.max_len (String.length s) in
        let v = Int32.to_int (Page.get_int32 page num_pos) in
        Printf.printf "[%s, %d]" s v;
        iterate iter)
      else ()
    in
    iterate iter

  let test1 =
    let file_manager =
      File.File_manager.make ~db_dirname:"db_test3" ~block_size:500
    in
    let log_file = "log_test3" in
    let log_manager = Log_manager.make ~file_manager ~log_file in
    create_records log_manager 1 10;
    print_log_records log_manager "The log file now has these records:";
    create_records log_manager 11 100;
    print_log_records log_manager "The log file now has these records:";
    "hello"
end

let test1 () = Alcotest.(check string) "same string" "hello" To_test.test1
let all_tests () = [ Alcotest.test_case "Test 1" `Quick test1 ]
