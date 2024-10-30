module To_test = struct
  open File
  module Schema = Record_page__Schema
  module Layout = Record_page__Layout
  module Expression = Predicate__Expression
  module Term = Predicate__Term
  module Constant = Constant

  let test_product_scan0 () =
    let fm = File_manager.make ~db_dirname:"tmp_productscan_test0" ~block_size:400 in
    let lm = Log_manager.make ~file_manager:fm ~log_file:"tmp_productscan_logs" in
    let bm = Buffer_manager.make ~file_manager:fm ~log_manager:lm ~num_buffers:8 () in
    let tx = Transaction.make ~file_manager:fm ~log_manager:lm ~buffer_manager:bm in
    
    let output = Buffer.create 1024 in
    
    (* Create first table (T1) and insert records *)
    let schema1 = Schema.make () in
    Schema.add_int_field schema1 "A";
    Schema.add_string_field schema1 "B" 9;
    let layout1 = Layout.make schema1 in
    let tbl_scan1 = Table_scan.make ~tx ~tbl_name:"T1" ~layout:layout1 in
    
    Buffer.add_string output "Inserting records into T1:\n";
    (* Insert records into T1 *)
    tbl_scan1#before_first;
    for i = 1 to 3 do
      tbl_scan1#insert;
      tbl_scan1#set_int32 ~field_name:"A" ~value:(Int32.of_int i);
      tbl_scan1#set_string ~field_name:"B" ~value:(Printf.sprintf "rec%d" i);
      Buffer.add_string output 
        (Printf.sprintf "T1: Inserted {A=%d, B=rec%d}\n" i i)
    done;

    (* Print T1 contents *)
    Buffer.add_string output "\nTable T1 contents:\n";
    tbl_scan1#before_first;
    while tbl_scan1#next do
      let a = Int32.to_int (tbl_scan1#get_int32 ~field_name:"A") in
      let b = tbl_scan1#get_string ~field_name:"B" in
      Buffer.add_string output (Printf.sprintf "T1: {A=%d, B=%s}\n" a b)
    done;

    (* Create second table (T2) and insert records *)
    let schema2 = Schema.make () in
    Schema.add_int_field schema2 "C";
    Schema.add_string_field schema2 "D" 9;
    let layout2 = Layout.make schema2 in
    let tbl_scan2 = Table_scan.make ~tx ~tbl_name:"T2" ~layout:layout2 in
    
    Buffer.add_string output "\nInserting records into T2:\n";
    (* Insert records into T2 *)
    tbl_scan2#before_first;
    for i = 1 to 2 do
      tbl_scan2#insert;
      tbl_scan2#set_int32 ~field_name:"C" ~value:(Int32.of_int (i * 10));
      tbl_scan2#set_string ~field_name:"D" ~value:(Printf.sprintf "val%d" i);
      Buffer.add_string output 
        (Printf.sprintf "T2: Inserted {C=%d, D=val%d}\n" (i * 10) i)
    done;

    (* Print T2 contents *)
    Buffer.add_string output "\nTable T2 contents:\n";
    tbl_scan2#before_first;
    while tbl_scan2#next do
      let c = Int32.to_int (tbl_scan2#get_int32 ~field_name:"C") in
      let d = tbl_scan2#get_string ~field_name:"D" in
      Buffer.add_string output (Printf.sprintf "T2: {C=%d, D=%s}\n" c d)
    done;

    (* Create predicate A = 2 for T1 *)
    let lhs = Expression.make_field_name "A" in
    let rhs = Expression.make_const (Constant.Integer (Int32.of_int 2)) in
    let term = Term.make lhs rhs in
    let pred = Predicate.make term in
    
    (* Create select scan on T1 *)
    let select = new Select_scan.t (tbl_scan1) pred in
    
    (* Print select scan results *)
    Buffer.add_string output "\nSelect scan results (T1 where A=2):\n";
    select#before_first;
    while select#next do
      let a = Int32.to_int (select#get_int32 ~field_name:"A") in
      let b = select#get_string ~field_name:"B" in
      Buffer.add_string output (Printf.sprintf "Selected: {A=%d, B=%s}\n" a b)
    done;
    
    (* Create product scan *)
    let product = new Product_scan.t (select) (tbl_scan2) in
    
    (* Test the product *)
    Buffer.add_string output "\nProduct results (Selected T1 × T2):\n";
    product#before_first;
    while product#next do
      let a = Int32.to_int (product#get_int32 ~field_name:"A") in
      let b = product#get_string ~field_name:"B" in
      let c = Int32.to_int (product#get_int32 ~field_name:"C") in
      let d = product#get_string ~field_name:"D" in
      Buffer.add_string output 
        (Printf.sprintf "{A=%d, B=%s, C=%d, D=%s}\n" a b c d)
    done;
    
    product#close;
    Transaction.commit tx;
    Buffer.contents output
end

let expected_output = "Inserting records into T1:
T1: Inserted {A=1, B=rec1}
T1: Inserted {A=2, B=rec2}
T1: Inserted {A=3, B=rec3}

Table T1 contents:
T1: {A=1, B=rec1}
T1: {A=2, B=rec2}
T1: {A=3, B=rec3}

Inserting records into T2:
T2: Inserted {C=10, D=val1}
T2: Inserted {C=20, D=val2}

Table T2 contents:
T2: {C=10, D=val1}
T2: {C=20, D=val2}

Select scan results (T1 where A=2):
Selected: {A=2, B=rec2}

Product results (Selected T1 × T2):
{A=2, B=rec2, C=10, D=val1}
{A=2, B=rec2, C=20, D=val2}
"

let test_product_scan0 () =
  Alcotest.(check string) "same output" expected_output (To_test.test_product_scan0 ())

let all_tests () =
  [ Alcotest.test_case "product scan basic ops" `Quick test_product_scan0 ]