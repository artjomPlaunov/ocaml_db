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
      Table_scan.set_int32 ~scan ~field_name:"A" ~value:(Int32.of_int (i - 1));
      Table_scan.set_string ~scan ~field_name:"B"
        ~value:(Printf.sprintf "rec %d" (i - 1));
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
    let module SelectScan = (val Scans.Select_scan.instantiate_select_scan pred_arg) in
    let sel_scan = SelectScan.make ~tx ~tbl_name:"T" ~layout in
    while SelectScan.next ~scan:sel_scan do 
      let a =
        Int32.to_int (SelectScan.get_int32 ~scan:sel_scan ~field_name:"A")
      in
      let b = SelectScan.get_string ~scan:sel_scan ~field_name:"B" in
      let rid_str =
        Record_id.to_string ~rid:(SelectScan.get_rid ~scan:sel_scan)
      in
      Printf.printf "slot %s: {%d, %s}\n" rid_str a b
    done;
    SelectScan.close ~scan:sel_scan;
    Transaction.commit tx;
    ""

  let test_product_scan1 () =
    let fm = File_manager.make ~db_dirname:"productscan_test1" ~block_size:300 in
    let lm = Log_manager.make ~file_manager:fm ~log_file:"productscan_logs" in
    let bm =
      Buffer_manager.make ~file_manager:fm ~log_manager:lm ~num_buffers:8 ()
    in
    let tx =
      Transaction.make ~file_manager:fm ~log_manager:lm ~buffer_manager:bm
    in
    (* Create first table with A, B fields *)
    let schema1 = Schema.make () in
    Schema.add_int_field schema1 "A";
    Schema.add_string_field schema1 "B" 9;
    let layout1 = Layout.make schema1 in
    let scan1 = Table_scan.make ~tx ~tbl_name:"T1" ~layout:layout1 in
    
    (* Create second table with C, D fields *)
    let schema2 = Schema.make () in
    Schema.add_int_field schema2 "C";
    Schema.add_string_field schema2 "D" 9;
    let layout2 = Layout.make schema2 in
    let scan2 = Table_scan.make ~tx ~tbl_name:"T2" ~layout:layout2 in

    (* Fill first table *)
    Printf.printf "Filling table T1 with records\n";
    Table_scan.before_first ~scan:scan1;
    for i = 1 to 3 do
      Table_scan.insert ~scan:scan1;
      Table_scan.set_int32 ~scan:scan1 ~field_name:"A" ~value:(Int32.of_int i);
      Table_scan.set_string ~scan:scan1 ~field_name:"B" ~value:(Printf.sprintf "b%d" i);
    done;

    (* Print contents of T1 *)
    Printf.printf "\nContents of table T1:\n";
    Table_scan.before_first ~scan:scan1;
    while Table_scan.next ~scan:scan1 do
      let a = Int32.to_int (Table_scan.get_int32 ~scan:scan1 ~field_name:"A") in
      let b = Table_scan.get_string ~scan:scan1 ~field_name:"B" in
      Printf.printf "{A=%d, B=%s}\n" a b
    done;

    (* Fill second table *)
    Printf.printf "\nFilling table T2 with records\n";
    Table_scan.before_first ~scan:scan2;
    for i = 1 to 2 do
      Table_scan.insert ~scan:scan2;
      Table_scan.set_int32 ~scan:scan2 ~field_name:"C" ~value:(Int32.of_int (i * 10));
      Table_scan.set_string ~scan:scan2 ~field_name:"D" ~value:(Printf.sprintf "d%d" i);
    done;

    (* Print contents of T2 *)
    Printf.printf "\nContents of table T2:\n";
    Table_scan.before_first ~scan:scan2;
    while Table_scan.next ~scan:scan2 do
      let c = Int32.to_int (Table_scan.get_int32 ~scan:scan2 ~field_name:"C") in
      let d = Table_scan.get_string ~scan:scan2 ~field_name:"D" in
      Printf.printf "{C=%d, D=%s}\n" c d
    done;

    (* Create product scan *)
    let scan1_arg = Scans.Product_scan.create_product_scan_arg scan1 in
    let scan2_arg = Scans.Product_scan.create_product_scan_arg scan2 in
    let module ProductScan = (val (Scans.Product_scan.instantiate_product_scan scan1_arg scan2_arg)) in

    Printf.printf "\nProduct scan results:\n";
    let dummy = ProductScan.make ~tx ~tbl_name:"" ~layout:layout1 in
    ProductScan.before_first ~scan:dummy;
    while ProductScan.next ~scan:dummy do
      let a = Int32.to_int (ProductScan.get_int32 ~scan:dummy ~field_name:"A") in
      let b = ProductScan.get_string ~scan:dummy ~field_name:"B" in
      let c = Int32.to_int (ProductScan.get_int32 ~scan:dummy ~field_name:"C") in
      let d = ProductScan.get_string ~scan:dummy ~field_name:"D" in
      Printf.printf "{A=%d, B=%s, C=%d, D=%s}\n" a b c d
    done;
    Table_scan.close ~scan:scan1;
    Table_scan.close ~scan:scan2;
    Transaction.commit tx;
    ""

  let test_product_select_scan () =
    let fm = File_manager.make ~db_dirname:"product_select_test" ~block_size:300 in
    let lm = Log_manager.make ~file_manager:fm ~log_file:"product_select_logs" in
    let bm =
      Buffer_manager.make ~file_manager:fm ~log_manager:lm ~num_buffers:8 ()
    in
    let tx =
      Transaction.make ~file_manager:fm ~log_manager:lm ~buffer_manager:bm
    in
    (* Create first table with A, B fields *)
    let schema1 = Schema.make () in
    Schema.add_int_field schema1 "A";
    Schema.add_string_field schema1 "B" 9;
    let layout1 = Layout.make schema1 in
    let scan1 = Table_scan.make ~tx ~tbl_name:"T1" ~layout:layout1 in
    
    (* Create second table with C, D fields *)
    let schema2 = Schema.make () in
    Schema.add_int_field schema2 "C";
    Schema.add_string_field schema2 "D" 9;
    let layout2 = Layout.make schema2 in
    let scan2 = Table_scan.make ~tx ~tbl_name:"T2" ~layout:layout2 in

    (* Fill first table *)
    Printf.printf "Filling table T1 with records\n";
    Table_scan.before_first ~scan:scan1;
    for i = 1 to 5 do
      Table_scan.insert ~scan:scan1;
      Table_scan.set_int32 ~scan:scan1 ~field_name:"A" ~value:(Int32.of_int i);
      Table_scan.set_string ~scan:scan1 ~field_name:"B" ~value:(Printf.sprintf "b%d" i);
    done;

    (* Fill second table *)
    Printf.printf "\nFilling table T2 with records\n";
    Table_scan.before_first ~scan:scan2;
    for i = 1 to 3 do
      Table_scan.insert ~scan:scan2;
      Table_scan.set_int32 ~scan:scan2 ~field_name:"C" ~value:(Int32.of_int (i * 10));
      Table_scan.set_string ~scan:scan2 ~field_name:"D" ~value:(Printf.sprintf "d%d" i);
    done;

    (* Create select scan for T1 where B = "b3" *)
    let lhs = Expression.make_field_name "B" in 
    let const = Constant.make_string "b3" in 
    let rhs = Expression.make_const const in 
    let term = Term.make lhs rhs in 
    let pred = Predicate.make term in 
    let pred_arg = Scans.Select_scan.create_predicate_arg pred in 
    let module SelectScan = (val Scans.Select_scan.instantiate_select_scan pred_arg) in
    let sel_scan = SelectScan.make ~tx ~tbl_name:"T1" ~layout:layout1 in

    (* Create product scan between select scan and T2 *)
    let scan1_arg = Scans.Product_scan.create_product_scan_arg (sel_scan) in
    let scan2_arg = Scans.Product_scan.create_product_scan_arg scan2 in
    let module ProductScan = (val (Scans.Product_scan.instantiate_product_scan scan1_arg scan2_arg)) in

    Printf.printf "\nProduct-Select scan results (T1.B='b3' * T2):\n";
    let product_scan = ProductScan.make ~tx ~tbl_name:"" ~layout:layout1 in
    while ProductScan.next ~scan:product_scan do
      let a = Int32.to_int (ProductScan.get_int32 ~scan:product_scan ~field_name:"A") in
      let b = ProductScan.get_string ~scan:product_scan ~field_name:"B" in
      let c = Int32.to_int (ProductScan.get_int32 ~scan:product_scan ~field_name:"C") in
      let d = ProductScan.get_string ~scan:product_scan ~field_name:"D" in
      Printf.printf "{A=%d, B=%s, C=%d, D=%s}\n" a b c d
    done;

    Table_scan.close ~scan:scan1;
    Table_scan.close ~scan:scan2;
    SelectScan.close ~scan:sel_scan;
    Transaction.commit tx;
    ""
end

let test_select_scan1 () =
  Alcotest.(check string)
    "test"
    "dummy"
    (To_test.test_select_scan1 ())

let test_product_scan1 () =
  Alcotest.(check string)
    "test"
    "dummy"
    (To_test.test_product_scan1 ())

let test_product_select_scan () =
  Alcotest.(check string)
    "test"
    "dummy"
    (To_test.test_product_select_scan ())

let all_tests () =
  [ (*Alcotest.test_case "test select scan1" `Quick test_select_scan1;*)
    Alcotest.test_case "test product scan1" `Quick test_product_scan1;
    Alcotest.test_case "test product select scan" `Quick test_product_select_scan ]
