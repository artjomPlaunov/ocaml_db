module To_test = struct
  open File
  module Schema = Record_page__Schema
  module Layout = Record_page__Layout
  module Expression = Scan__Expression
  module Predicate = Scan__Predicate
  module Term = Scan__Term


  let test_select_scan1 () =
    let fm = File_manager.make ~db_dirname:"selectscan_test0" ~block_size:300 in
    let lm = Log_manager.make ~file_manager:fm ~log_file:"selectscan_logs" in
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
    let scan = Table_scan.make ~tx ~tbl_name:"T" ~layout in
    Printf.printf "filling the table with 50 random records.\n";
    Table_scan.before_first ~scan;
    for i = 1 to 50 do
      Table_scan.insert ~scan;
      Table_scan.set_int32 ~scan ~field_name:"A"
        ~value:(Int32.of_int (i-1));
      Table_scan.set_string ~scan ~field_name:"B"
        ~value:(Printf.sprintf "rec %d" (i-1));
      let rid_str = Record_id.to_string ~rid:(Table_scan.get_rid ~scan) in
      Printf.printf "inserting into slot %s: {%d, rec %d}\n" rid_str i i
    done;
    Printf.printf "Here the records:\n";
    Table_scan.before_first ~scan;
    while Table_scan.next ~scan do
      let a = Int32.to_int (Table_scan.get_int32 ~scan ~field_name:"A") in
      let b = Table_scan.get_string ~scan ~field_name:"B" in
      let rid_str = Record_id.to_string ~rid:(Table_scan.get_rid ~scan) in
      Printf.printf "slot %s: {%d, %s}\n" rid_str a b
    done;
    Table_scan.close ~scan;
    let lhs = Expression.make_field_name "B" in 
    let const = Constant.make_string "rec 45" in 
    let rhs = Expression.make_const const in 
    let term = Term.make lhs rhs in 
    let pred : Scan__Predicate.t = Predicate.make term in 
    let pred_arg = Scans.Select_scan.create_predicate_arg pred in 
    let module SelectScan = (val (Scans.Select_scan.instantiate_select_scan pred_arg)) in
    let sel_scan = SelectScan.make ~tx ~tbl_name:"T" ~layout in
    while SelectScan.next ~scan:sel_scan do 
      let a = Int32.to_int (SelectScan.get_int32 ~scan:sel_scan ~field_name:"A") in
      let b = SelectScan.get_string ~scan:sel_scan ~field_name:"B" in
      let rid_str = Record_id.to_string ~rid:(SelectScan.get_rid ~scan:sel_scan) in
      Printf.printf "slot %s: {%d, %s}\n" rid_str a b
    done;
    SelectScan.close ~scan:sel_scan;
    Transaction.commit tx;
    ""
end

let test_select_scan1 () =
  Alcotest.(check string)
    "test"
    "dummy"
    (To_test.test_select_scan1 ())

let all_tests () =
  [ Alcotest.test_case "test select scan1" `Quick test_select_scan1 ]
