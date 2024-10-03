module To_test = struct
  open File

  let create_log_record log_msg log_num =
    let num_pos = Page.max_len (String.length log_msg) in
    let block_size = num_pos + 4 in
    let page = Page.make ~block_size in
    Page.set_string page 0 log_msg;
    Page.set_int32 page num_pos (Int32.of_int log_num);
    page

  let create_records_in_log log_mgr start end_ =
    Printf.printf "Creating records: ";
    for i = start to end_ do
      let record = create_log_record ("record" ^ string_of_int i) i in
      let lsn = Log_manager.append log_mgr (Page.contents record) in
      Printf.printf "%d " lsn
    done;
    Printf.printf "\n"

  let print_log_records_with_message log_mgr msg =
    Printf.printf "%s\n" msg;
    let iterator = Log_manager.get_iterator log_mgr in
    let rec iterate_records iterator =
      if Log_manager__Log_iterator.has_next iterator then (
        let bytes = Log_manager__Log_iterator.next iterator in
        let page = Page.from_bytes bytes in
        let record_message = Page.get_string page 0 in
        let num_pos = Page.max_len (String.length record_message) in
        let log_number = Int32.to_int (Page.get_int32 page num_pos) in
        Printf.printf "[%s, %d]" record_message log_number;
        iterate_records iterator)
      else ()
    in
    iterate_records iterator

  let create_logs () =
    let file_manager =
      File_manager.make ~db_dirname:"db_test_create_logs" ~block_size:500
    in
    let log_file_name = "log_test_create_logs" in
    let log_manager = Log_manager.make ~file_manager ~log_file:log_file_name in
    create_records_in_log log_manager 1 10;
    print_log_records_with_message log_manager
      "The log file now has these records:";
    create_records_in_log log_manager 11 100;
    print_log_records_with_message log_manager
      "The log file now has these records:"
end

let test_create_logs () = To_test.create_logs ()
let all_tests () = [ Alcotest.test_case "create logs" `Quick test_create_logs ]
