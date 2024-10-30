module To_test = struct
  open File
  module Schema = Record_page__Schema
  module Layout = Record_page__Layout
  module Expression = Predicate__Expression
  module Term = Predicate__Term
  module Constant = Constant

  let test_select_select0 () =
    let fm = File_manager.make ~db_dirname:"tmp_selectselect_test0" ~block_size:400 in
    let lm = Log_manager.make ~file_manager:fm ~log_file:"tmp_selectselect_logs" in
    let bm = Buffer_manager.make ~file_manager:fm ~log_manager:lm ~num_buffers:8 () in
    let tx = Transaction.make ~file_manager:fm ~log_manager:lm ~buffer_manager:bm in
    
    (* Create table and insert records *)
    let schema = Schema.make () in
    Schema.add_int_field schema "A";
    Schema.add_int_field schema "B";
    Schema.add_string_field schema "C" 9;
    let layout = Layout.make schema in
    let tbl_scan = Table_scan.make ~tx ~tbl_name:"T" ~layout in
    
    let output = Buffer.create 1024 in
    Buffer.add_string output "Initial records:\n";
    
    (* Insert records *)
    tbl_scan#before_first;
    for i = 0 to 5 do
      tbl_scan#insert;
      tbl_scan#set_int32 ~field_name:"A" ~value:(Int32.of_int (i mod 3));
      tbl_scan#set_int32 ~field_name:"B" ~value:(Int32.of_int (i mod 2));
      tbl_scan#set_string ~field_name:"C" ~value:(Printf.sprintf "rec%d" (i+1));
      Buffer.add_string output 
        (Printf.sprintf "Inserted: {A=%d, B=%d, C=rec%d}\n" (i mod 3) (i mod 2) (i+1))
    done;

    (* Print all records before filtering *)
    Buffer.add_string output "\nAll records before filtering:\n";
    tbl_scan#before_first;
    while tbl_scan#next do
      let a = Int32.to_int (tbl_scan#get_int32 ~field_name:"A") in
      let b = Int32.to_int (tbl_scan#get_int32 ~field_name:"B") in
      let c = tbl_scan#get_string ~field_name:"C" in
      Buffer.add_string output (Printf.sprintf "{A=%d, B=%d, C=%s}\n" a b c)
    done;

    (* Create first predicate: A = 2 *)
    let lhs1 = Expression.make_field_name "A" in
    let rhs1 = Expression.make_const (Constant.Integer (Int32.of_int 2)) in
    let term1 = Term.make lhs1 rhs1 in
    let pred1 = Predicate.make term1 in
    
    (* Create first select scan *)
    let select1 = new Select_scan.t (tbl_scan) pred1 in
    
    (* Print first selection results *)
    Buffer.add_string output "\nFirst selection (A = 2):\n";
    select1#before_first;
    while select1#next do
      let a = Int32.to_int (select1#get_int32 ~field_name:"A") in
      let b = Int32.to_int (select1#get_int32 ~field_name:"B") in
      let c = select1#get_string ~field_name:"C" in
      Buffer.add_string output (Printf.sprintf "{A=%d, B=%d, C=%s}\n" a b c)
    done;
    
    (* Create second predicate: B = 1 *)
    let lhs2 = Expression.make_field_name "B" in
    let rhs2 = Expression.make_const (Constant.Integer (Int32.of_int 1)) in
    let term2 = Term.make lhs2 rhs2 in
    let pred2 = Predicate.make term2 in

    (* Create second select scan *)
    let select2 = new Select_scan.t (select1) pred2 in
    
    (* Test the nested selection *)
    Buffer.add_string output "\nSecond selection (A = 2 AND B = 1):\n";
    select2#before_first;
    while select2#next do
      let a = Int32.to_int (select2#get_int32 ~field_name:"A") in
      let b = Int32.to_int (select2#get_int32 ~field_name:"B") in
      let c = select2#get_string ~field_name:"C" in
      Buffer.add_string output (Printf.sprintf "{A=%d, B=%d, C=%s}\n" a b c)
    done;
    
    select2#close;
    Transaction.commit tx;
    Buffer.contents output
end

let expected_output = "Initial records:
Inserted: {A=0, B=0, C=rec1}
Inserted: {A=1, B=1, C=rec2}
Inserted: {A=2, B=0, C=rec3}
Inserted: {A=0, B=1, C=rec4}
Inserted: {A=1, B=0, C=rec5}
Inserted: {A=2, B=1, C=rec6}

All records before filtering:
{A=0, B=0, C=rec1}
{A=1, B=1, C=rec2}
{A=2, B=0, C=rec3}
{A=0, B=1, C=rec4}
{A=1, B=0, C=rec5}
{A=2, B=1, C=rec6}

First selection (A = 2):
{A=2, B=0, C=rec3}
{A=2, B=1, C=rec6}

Second selection (A = 2 AND B = 1):
{A=2, B=1, C=rec6}
"

let test_select_select0 () =
  Alcotest.(check string) "same output" expected_output (To_test.test_select_select0 ())

let all_tests () =
  [ Alcotest.test_case "nested select scans" `Quick test_select_select0 ]