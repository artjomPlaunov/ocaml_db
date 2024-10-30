module To_test = struct
  open File
  module Schema = Record_page__Schema
  module Layout = Record_page__Layout
  module Expression = Predicate__Expression
  module Term = Predicate__Term
  module Constant = Constant

  let test_select_scan0 () =
    let fm = File_manager.make ~db_dirname:"selectscan_test0" ~block_size:400 in
    let lm = Log_manager.make ~file_manager:fm ~log_file:"selectscan_logs" in
    let bm = Buffer_manager.make ~file_manager:fm ~log_manager:lm ~num_buffers:8 () in
    let tx = Transaction.make ~file_manager:fm ~log_manager:lm ~buffer_manager:bm in
    
    (* Create table and insert records *)
    let schema = Schema.make () in
    Schema.add_int_field schema "A";
    Schema.add_string_field schema "B" 9;
    let layout = Layout.make schema in
    let tbl_scan = Table_scan.make ~tx ~tbl_name:"T" ~layout in
    
    (* Insert some test records *)
    tbl_scan#before_first;
    for i = 1 to 50 do
      tbl_scan#insert;
      tbl_scan#set_int32 ~field_name:"A" ~value:(Int32.of_int i);
      tbl_scan#set_string ~field_name:"B" ~value:(Printf.sprintf "rec%d" i)
    done;

    (* Print all records before filtering *)
    Printf.printf "All records before filtering:\n";
    tbl_scan#before_first;
    while tbl_scan#next do
      let a = Int32.to_int (tbl_scan#get_int32 ~field_name:"A") in
      let b = tbl_scan#get_string ~field_name:"B" in
      Printf.printf "record: {%d, %s}\n" a b
    done;

    (* Create predicate A = 25 *)
    let lhs = Expression.make_field_name "A" in
    let rhs = Expression.make_const (Constant.Integer (Int32.of_int 25)) in
    let term = Term.make lhs rhs in
    let pred = Predicate.make term in
    
    (* Create select scan *)
    let select = new Select_scan.t (tbl_scan) pred in
    
    (* Test the selection *)
    Printf.printf "\nFiltered records (A = 25):\n";
    select#before_first;
    let found = ref false in
    while select#next do
      let a = Int32.to_int (select#get_int32 ~field_name:"A") in
      let b = select#get_string ~field_name:"B" in
      Printf.printf "found {%d, %s}\n" a b;
      found := true
    done;
    
    select#close;
    tbl_scan#close;
    Transaction.commit tx;
    
    if !found then "" else "No matching records found"
end

let test_select_scan0 () =
  Alcotest.(check string) "string equality" "" (To_test.test_select_scan0 ())

let all_tests () =
  [ Alcotest.test_case "select scan test0" `Quick test_select_scan0 ]