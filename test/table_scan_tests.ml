module To_test = struct
  open File
  module Schema = Record_page__Schema
  module Layout = Record_page__Layout

  let test_table_scan0 () =
    let fm = File_manager.make ~db_dirname:"tablescan_test0" ~block_size:300 in
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
    let tbl_scan = Table_scan.make ~tx ~tbl_name:"T" ~layout in
    Printf.printf "filling the table with 50 random records.\n";
    Table_scan.before_first ~tbl_scan;
    for i = 1 to 50 do
      Table_scan.insert ~tbl_scan;
      Table_scan.set_int32 ~tbl_scan ~field_name:"A"
        ~value:(Int32.of_int (i - 1));
      Table_scan.set_string ~tbl_scan ~field_name:"B"
        ~value:(Printf.sprintf "rec %d" i);
      let rid_str = Record_id.to_string ~rid:(Table_scan.get_rid ~tbl_scan) in
      Printf.printf "inserting into slot %s: {%d, rec %d}\n" rid_str i i
    done;
    Printf.printf "Deleting records with A-vals < 25.\n";
    Table_scan.before_first ~tbl_scan;
    while Table_scan.next ~tbl_scan do
      let a = Int32.to_int (Table_scan.get_int32 ~tbl_scan ~field_name:"A") in
      let b = Table_scan.get_string ~tbl_scan ~field_name:"B" in
      if a < 25 then (
        let rid_str = Record_id.to_string ~rid:(Table_scan.get_rid ~tbl_scan) in
        Printf.printf "Deleting slot %s: {%d, %s}\n" rid_str a b;
        Table_scan.delete ~tbl_scan)
    done;
    Printf.printf "Here are the remaining records:\n";
    Table_scan.before_first ~tbl_scan;
    while Table_scan.next ~tbl_scan do
      let a = Int32.to_int (Table_scan.get_int32 ~tbl_scan ~field_name:"A") in
      let b = Table_scan.get_string ~tbl_scan ~field_name:"B" in
      let rid_str = Record_id.to_string ~rid:(Table_scan.get_rid ~tbl_scan) in
      Printf.printf "slot %s: {%d, %s}\n" rid_str a b
    done;
    Table_scan.close ~tbl_scan;
    Transaction.commit tx;
    ""
end

let test_table_scan0 () =
  Alcotest.(check string)
    "string equality" "<START 1><UPDATE INT 1 testfile, 1 80 255><COMMIT 1>"
    (To_test.test_table_scan0 ())

let all_tests () =
  [ Alcotest.test_case "table scan test0" `Quick test_table_scan0 ]
