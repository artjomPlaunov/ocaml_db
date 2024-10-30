module To_test = struct
  open File
  module Schema = Record_page__Schema
  module Layout = Record_page__Layout

  let test_table_scan0 () =
    let fm = File_manager.make ~db_dirname:"tmp_tablescan_test0" ~block_size:300 in
    let lm = Log_manager.make ~file_manager:fm ~log_file:"tmp_tblscan_logs" in
    let bm = Buffer_manager.make ~file_manager:fm ~log_manager:lm ~num_buffers:8 () in
    let tx = Transaction.make ~file_manager:fm ~log_manager:lm ~buffer_manager:bm in
    
    let schema = Schema.make () in
    Schema.add_int_field schema "A";
    Schema.add_string_field schema "B" 9;
    let layout = Layout.make schema in
    let tbl_scan = Table_scan.make ~tx ~tbl_name:"T" ~layout in
    
    let output = Buffer.create 1024 in
    Buffer.add_string output "Initial records:\n";
    
    (* Insert records *)
    tbl_scan#before_first;
    for i = 1 to 5 do
      tbl_scan#insert;
      tbl_scan#set_int32 ~field_name:"A" ~value:(Int32.of_int i);
      tbl_scan#set_string ~field_name:"B" ~value:(Printf.sprintf "rec%d" i);
      Buffer.add_string output (Printf.sprintf "Inserted: {A=%d, B=rec%d}\n" i i)
    done;

    (* Print current records *)
    Buffer.add_string output "\nCurrent records:\n";
    tbl_scan#before_first;
    while tbl_scan#next do
      let a = Int32.to_int (tbl_scan#get_int32 ~field_name:"A") in
      let b = tbl_scan#get_string ~field_name:"B" in
      Buffer.add_string output (Printf.sprintf "{A=%d, B=%s}\n" a b)
    done;

    tbl_scan#close;
    Transaction.commit tx;
    Buffer.contents output
end

let expected_output = "Initial records:
Inserted: {A=1, B=rec1}
Inserted: {A=2, B=rec2}
Inserted: {A=3, B=rec3}
Inserted: {A=4, B=rec4}
Inserted: {A=5, B=rec5}

Current records:
{A=1, B=rec1}
{A=2, B=rec2}
{A=3, B=rec3}
{A=4, B=rec4}
{A=5, B=rec5}
"

let test_table_scan0 () =
  Alcotest.(check string) "same output" expected_output (To_test.test_table_scan0 ())

let all_tests () =
  [ Alcotest.test_case "table scan basic ops" `Quick test_table_scan0 ]
