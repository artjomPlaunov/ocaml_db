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
    for i = start to end_ do
      let record = create_log_record ("record" ^ string_of_int i) i in
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
    intro_msg ^ iterate_records iterator s

  let create_logs () =
    let file_manager =
      File_manager.make ~db_dirname:"db_test_create_logs" ~block_size:500
    in
    let log_file_name = "log_test_create_logs" in
    let log_manager = Log_manager.make ~file_manager ~log_file:log_file_name in
    create_records_in_log log_manager 1 10;
    let s1 =
      print_log_records_with_message log_manager
        "The log file now has these records:"
    in
    create_records_in_log log_manager 11 100;
    let s2 =
      print_log_records_with_message log_manager
        "The log file now has these records:"
    in
    s1 ^ s2

  let test_append_mutex () =
    let file_manager =
      File_manager.make ~db_dirname:"db_test_create_logs2" ~block_size:500
    in
    let log_file_name = "log_test_create_logs" in
    let log_manager = Log_manager.make ~file_manager ~log_file:log_file_name in
    let threadA = Thread.create (create_records_in_log log_manager 1) 10000 in
    let threadB =
      Thread.create (create_records_in_log log_manager 20000) 30000
    in
    Thread.join threadB;
    Thread.join threadA;
    let s = print_log_records_with_message log_manager "thread A records" in
    Printf.printf "%s\n" s;
    "unpredictable thread output"
end

let test1_expected_string =
  "The log file now has these records:\n\
   [record1, 1][record2, 2][record3, 3][record4, 4][record5, 5][record6, \
   6][record7, 7][record8, 8][record9, 9][record10, 10]The log file now has \
   these records:\n\
   [record1, 1][record2, 2][record3, 3][record4, 4][record5, 5][record6, \
   6][record7, 7][record8, 8][record9, 9][record10, 10][record11, \
   11][record12, 12][record13, 13][record14, 14][record15, 15][record16, \
   16][record17, 17][record18, 18][record19, 19][record20, 20][record21, \
   21][record22, 22][record23, 23][record24, 24][record25, 25][record26, \
   26][record27, 27][record28, 28][record29, 29][record30, 30][record31, \
   31][record32, 32][record33, 33][record34, 34][record35, 35][record36, \
   36][record37, 37][record38, 38][record39, 39][record40, 40][record41, \
   41][record42, 42][record43, 43][record44, 44][record45, 45][record46, \
   46][record47, 47][record48, 48][record49, 49][record50, 50][record51, \
   51][record52, 52][record53, 53][record54, 54][record55, 55][record56, \
   56][record57, 57][record58, 58][record59, 59][record60, 60][record61, \
   61][record62, 62][record63, 63][record64, 64][record65, 65][record66, \
   66][record67, 67][record68, 68][record69, 69][record70, 70][record71, \
   71][record72, 72][record73, 73][record74, 74][record75, 75][record76, \
   76][record77, 77][record78, 78][record79, 79][record80, 80][record81, \
   81][record82, 82][record83, 83][record84, 84][record85, 85][record86, \
   86][record87, 87][record88, 88][record89, 89][record90, 90][record91, \
   91][record92, 92][record93, 93][record94, 94][record95, 95][record96, \
   96][record97, 97][record98, 98][record99, 99][record100, 100]"

let test_create_logs () =
  Alcotest.(check string)
    "same string" test1_expected_string (To_test.create_logs ())

let test_append_mutex () =
  Alcotest.(check string)
    "same string" "unpredictable thread output"
    (To_test.test_append_mutex ())

let all_tests () =
  [
    Alcotest.test_case "create logs" `Quick test_create_logs;
    Alcotest.test_case "append mutex" `Slow test_append_mutex;
  ]
