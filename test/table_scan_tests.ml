module To_test = struct
  open File
  module Schema = Record_page__Schema
  module Layout = Record_page__Layout

  let test_table_scan0 () =
    let fm =
      File_manager.make ~db_dirname:"tablescan_test0" ~block_size:1024
    in
    let lm = Log_manager.make ~file_manager:fm ~log_file:"tblscan_logs" in
    let bm =
      Buffer_manager.make ~file_manager:fm ~log_manager:lm ~num_buffers:8 ()
    in
    let tx =
      Transaction.make ~file_manager:fm ~log_manager:lm ~buffer_manager:bm
    in
    let schema = Schema.make () in
    Schema.add_int_field schema "A";
    Schema.add_string_field schema "B" 9;
    let layout = Layout.make schema in
    let iter_f field_name =
      let offset = Layout.get_offset layout field_name in
      Printf.printf "%s has offset %d\n" field_name offset
    in
    List.iter iter_f (Schema.fields (Layout.get_schema layout));
    
    ""
end

let test_table_scan0 () =
  Alcotest.(check string)
    "string equality" "<START 1><UPDATE INT 1 testfile, 1 80 255><COMMIT 1>"
    (To_test.test_table_scan0 ())

let all_tests () =
  [ Alcotest.test_case "table scan test0" `Quick test_table_scan0]
