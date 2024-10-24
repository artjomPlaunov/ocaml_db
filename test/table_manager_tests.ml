module To_test = struct
  open File
  module Schema = Record_page__Schema
  module Layout = Record_page__Layout

  let test_tbl_mgr_1 () =
    let fm =
      File_manager.make ~db_dirname:"based_table_manager_test" ~block_size:1024 in
    let lm = Log_manager.make ~file_manager:fm ~log_file:"based_logs" in
    let bm =
      Buffer_manager.make ~file_manager:fm ~log_manager:lm ~num_buffers:8 ()
    in
    let tx =
      Transaction.make ~file_manager:fm ~log_manager:lm ~buffer_manager:bm
    in
    let tm = 
      Table_manager.make ~is_new:true ~tx in 
    let schema = Schema.make () in 
    Schema.add_int_field schema "A";
    Schema.add_string_field schema "B" 9;
    Table_manager.create_table tm "MyTable" schema tx;
    Printf.printf "All tables and their lengths: \n";
    let layout = Table_manager.get_layout tm "tablecatalog" tx in



    let tm = Table_manager.create_table in 
    ""

 
end


let test_tbl_mgr_1 () =
  Alcotest.(check string)
    "same string" "<UPDATE INT 15 fname, 1 255 15>"
    (To_test.test_tbl_mgr_1 ())


let all_tests () =
  [
    Alcotest.test_case "create int logs" `Quick test_tbl_mgr_1;
  ]
