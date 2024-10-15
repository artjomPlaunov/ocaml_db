module To_test = struct
  open File
  module Schema = Record_page__Schema
  module Layout = Record_page__Layout

  let test_record_page0 () =
    let fm =
      File_manager.make ~db_dirname:"record_page_test0" ~block_size:1024
    in
    let lm = Log_manager.make ~file_manager:fm ~log_file:"recordpage_logs" in
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
    let block = Transaction.append ~tx ~filename:"testfile" in
    Transaction.pin ~tx ~block;
    let rec_page = Record_page.make tx block layout in
    Record_page.format rec_page; 
    ""
end

let test_record_page0 () =
  Alcotest.(check string)
    "string equality" "<START 1><UPDATE INT 1 testfile, 1 80 255><COMMIT 1>"
    (To_test.test_record_page0 ())

let all_tests () =
  [
    Alcotest.test_case "record page test0" `Quick test_record_page0;
  ]
