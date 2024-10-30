module To_test = struct
  open File
  module Schema = Record_page__Schema
  module Layout = Record_page__Layout
  module Expression = Predicate__Expression
  module Term = Predicate__Term
  module Constant = Constant

  let test_select_select0 () =
    let fm = File_manager.make ~db_dirname:"selectselect_test0" ~block_size:400 in
    let lm = Log_manager.make ~file_manager:fm ~log_file:"selectselect_logs" in
    let bm = Buffer_manager.make ~file_manager:fm ~log_manager:lm ~num_buffers:8 () in
    let tx = Transaction.make ~file_manager:fm ~log_manager:lm ~buffer_manager:bm in
    
    (* Create table and insert records *)
    let schema = Schema.make () in
    Schema.add_int_field schema "A";
    Schema.add_int_field schema "B";
    Schema.add_string_field schema "C" 9;
    let layout = Layout.make schema in
    let tbl_scan = Table_scan.make ~tx ~tbl_name:"T" ~layout in
    
    (* Insert some test records *)
    tbl_scan#before_first;
    for i = 1 to 50 do
      tbl_scan#insert;
      tbl_scan#set_int32 ~field_name:"A" ~value:(Int32.of_int (i mod 5));
      tbl_scan#set_int32 ~field_name:"B" ~value:(Int32.of_int (i mod 3));
      tbl_scan#set_string ~field_name:"C" ~value:(Printf.sprintf "rec%d" i)
    done;

    (* Print all records before filtering *)
    Printf.printf "All records before filtering:\n";
    tbl_scan#before_first;
    while tbl_scan#next do
      let a = Int32.to_int (tbl_scan#get_int32 ~field_name:"A") in
      let b = Int32.to_int (tbl_scan#get_int32 ~field_name:"B") in
      let c = tbl_scan#get_string ~field_name:"C" in
      Printf.printf "record: {A=%d, B=%d, C=%s}\n" a b c
    done;

    (* Create first predicate: A = 2 *)
    let lhs1 = Expression.make_field_name "A" in
    let rhs1 = Expression.make_const (Constant.Integer (Int32.of_int 2)) in
    let term1 = Term.make lhs1 rhs1 in
    let pred1 = Predicate.make term1 in
    
    (* Create first select scan *)
    let select1 = new Select_scan.t (tbl_scan) pred1 in
    
    (* Create second predicate: B = 1 *)
    let lhs2 = Expression.make_field_name "B" in
    let rhs2 = Expression.make_const (Constant.Integer (Int32.of_int 1)) in
    let term2 = Term.make lhs2 rhs2 in
    let pred2 = Predicate.make term2 in

    (* Create second select scan on top of first *)
    let select2 = new Select_scan.t (select1) pred2 in
    
    (* Test the nested selection *)
    Printf.printf "\nFiltered records (A = 2 AND B = 1):\n";
    select2#before_first;
    let found = ref false in
    while select2#next do
      let a = Int32.to_int (select2#get_int32 ~field_name:"A") in
      let b = Int32.to_int (select2#get_int32 ~field_name:"B") in
      let c = select2#get_string ~field_name:"C" in
      Printf.printf "found {A=%d, B=%d, C=%s}\n" a b c;
      found := true
    done;
    
    select2#close;
    Transaction.commit tx;
    
    if !found then "" else "No matching records found"
end

let test_select_select0 () =
  Alcotest.(check string) "string equality" "" (To_test.test_select_select0 ())

let all_tests () =
  [ Alcotest.test_case "select select test0" `Quick test_select_select0 ] 