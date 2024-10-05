module To_test = struct
  open File

  (* let create_log_record log_num =
    let num_pos = Page.max_len (String.length log_msg) in
    let block_size = num_pos + 4 in
    let page = Page.make ~block_size in
    Page.set_string page 0 log_msg;
    Page.set_int32 page num_pos (Int32.of_int log_num);
    page

  let create_records_in_log log_mgr start end_ =
    for i = start to end_ do
      let record = create_int_log_record i in
      let _ = Log_manager.append log_mgr (Page.contents record) in
      ()
    done

  let print_log_records_with_message log_mgr msg =
    let intro_msg = Printf.sprintf "%s\n" msg in
    let iterator = Log_manager.get_iterator log_mgr in
    let s = "" in
    let rec iterate_records iterator s1 =
      if Log_manager__Log_iterator.has_next iterator then
        let bytes = Log_manager__Log_iterator.next iterator in
        let page = Page.from_bytes bytes in
        let record_message = Page.get_string page 0 in
        let num_pos = Page.max_len (String.length record_message) in
        let log_number = Int32.to_int (Page.get_int32 page num_pos) in
        let s2 = Printf.sprintf "[%s, %d]" record_message log_number in
        iterate_records iterator s1 ^ s2
      else s1
    in
    intro_msg ^ iterate_records iterator s *)

  let create_int_logs = "todo"
    (* let file_manager =
      File_manager.make ~db_dirname:"db_test_create_int_logs" ~block_size:500
    in
    let log_file_name = "log_test_create_logs" in
    let log_manager = Log_manager.make ~file_manager ~log_file:log_file_name in
    create_int_records_in_log log_manager 1 10;
    let s1 =
      print_log_records_with_message log_manager
        "The log file now has these records:"
    in
    create_records_in_log log_manager 11 100;
    let s2 =
      print_log_records_with_message log_manager
        "The log file now has these records:"
    in
    s1 ^ s2 *)
end

let test_create_logs () =
  Alcotest.(check string)
    "same string" "lol" To_test.create_int_logs

let all_tests () = [ Alcotest.test_case "create logs" `Quick test_create_logs ]
