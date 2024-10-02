module To_test = struct
  let create_log_record str num =
    let num_pos = File.Page.max_len (String.length str) in
    let block_size = num_pos + 4 in
    let page = File.Page.make ~block_size in
    let _ = File.Page.set_string page 0 str in
    let _ = File.Page.set_int32 page num_pos (Int32.of_int num) in
    page

  let create_records log_manager start end_ =
    let _ = Printf.printf "Creating records: " in
    let _ =
      for i = start to end_ do
        let record = create_log_record ("record" ^ string_of_int i) i in
        let lsn = Log_manager.append log_manager (File.Page.contents record) in
        Printf.printf "%d " lsn
      done
    in
    Printf.printf "\n"

  let print_log_records log_mgr msg =
    let _ = Printf.printf "%s\n" msg in
    let iter = Log_manager.get_iterator log_mgr in
    let rec iterate iter =
      if Log_manager__Log_iterator.has_next iter then
        let b = Log_manager__Log_iterator.next iter in
        let page = File.Page.from_bytes b in
        let s = File.Page.get_string page 0 in
        let num_pos = File.Page.max_len (String.length s) in
        let v = Int32.to_int (File.Page.get_int32 page num_pos) in
        let _ = Printf.printf "[%s, %d]" s v in
        iterate iter
      else ()
    in
    iterate iter

  let test3 =
    let file_manager =
      File.File_manager.make ~db_dirname:"db_test3" ~block_size:100
    in
    let log_file = "log_test3" in
    let log_manager = Log_manager.make ~file_manager ~log_file in
    let _ = create_records log_manager 1 10 in
    let _ =
      print_log_records log_manager "The log file now has these records:"
    in
    let _ = create_records log_manager 11 20 in
    let _ =
      print_log_records log_manager "The log file now has these records:"
    in
    "hello"
end

let test3 () = Alcotest.(check string) "same string" "hello" To_test.test3
