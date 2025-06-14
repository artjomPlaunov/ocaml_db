module To_test = struct
  open File_manager
  open Test_utils
  module Schema = Record_page.Schema
  module Layout = Record_page.Layout
  module Constant = Constant
  module Predicate = Scans__Predicate
  module Expression = Scans__Expression
  module Term = Scans__Term
  module Table_scan = Scans__Table_scan
  module Select_scan = Scans__Select_scan
  module Project_scan = Scans__Project_scan
  module Product_scan = Scans__Product_scan

  (* Table Scan Test *)
  let test_table_scan0 () =
    let env = make_test_env ~db_name:"tablescan_test0" in
    let schema = Schema.make () in
    Schema.add_int_field schema "A";
    Schema.add_string_field schema "B" 9;
    let layout = Layout.make schema in
    let tbl_scan = Table_scan.make ~tx:env.transaction ~tbl_name:"T" ~layout in

    Buffer.add_string env.output "Initial records:\n";

    tbl_scan#before_first;
    for i = 1 to 3 do
      tbl_scan#insert;
      tbl_scan#set_int32 ~field_name:"A" ~value:(Int32.of_int i);
      tbl_scan#set_string ~field_name:"B" ~value:(Printf.sprintf "rec%d" i);
      Buffer.add_string env.output
        (Printf.sprintf "Inserted: {A=%d, B=rec%d}\n" i i)
    done;

    cleanup_test_env tbl_scan env;
    let output = Buffer.contents env.output in
    write_test_output ~test_name:"table_scan" ~output ~db_name:"tablescan_test0";
    output

  (* Select Scan Test *)
  let test_select_scan0 () =
    let env = make_test_env ~db_name:"selectscan_test0" in
    let schema = Schema.make () in
    Schema.add_int_field schema "A";
    Schema.add_string_field schema "B" 9;
    let layout = Layout.make schema in
    let tbl_scan = Table_scan.make ~tx:env.transaction ~tbl_name:"T" ~layout in

    Buffer.add_string env.output "Initial records:\n";

    tbl_scan#before_first;
    for i = 1 to 5 do
      tbl_scan#insert;
      tbl_scan#set_int32 ~field_name:"A" ~value:(Int32.of_int i);
      tbl_scan#set_string ~field_name:"B" ~value:(Printf.sprintf "rec%d" i);
      Buffer.add_string env.output
        (Printf.sprintf "Inserted: {A=%d, B=rec%d}\n" i i)
    done;

    let get_fields scan =
      let a = Int32.to_int (scan#get_int32 ~field_name:"A") in
      let b = scan#get_string ~field_name:"B" in
      Printf.sprintf "{A=%d, B=%s}\n" a b
    in
    print_table_contents ~output:env.output ~name:"All records before filtering"
      ~scan:tbl_scan ~get_fields;

    let pred = make_predicate "A" 3 in
    let select = new Select_scan.t tbl_scan pred in

    print_table_contents ~output:env.output ~name:"Filtered records (A = 3)"
      ~scan:select ~get_fields;

    cleanup_test_env select env;
    let output = Buffer.contents env.output in
    write_test_output ~test_name:"select_scan" ~output
      ~db_name:"selectscan_test0";
    output

  (* Project Scan Test *)
  let test_project_scan0 () =
    let env = make_test_env ~db_name:"projectscan_test0" in
    let schema = Schema.make () in
    Schema.add_int_field schema "A";
    Schema.add_string_field schema "B" 9;
    Schema.add_int_field schema "C";
    let layout = Layout.make schema in
    let tbl_scan = Table_scan.make ~tx:env.transaction ~tbl_name:"T" ~layout in

    Buffer.add_string env.output "Initial records:\n";

    tbl_scan#before_first;
    for i = 1 to 5 do
      tbl_scan#insert;
      tbl_scan#set_int32 ~field_name:"A" ~value:(Int32.of_int i);
      tbl_scan#set_string ~field_name:"B" ~value:(Printf.sprintf "rec%d" i);
      tbl_scan#set_int32 ~field_name:"C" ~value:(Int32.of_int (i * 10));
      Buffer.add_string env.output
        (Printf.sprintf "Inserted: {A=%d, B=rec%d, C=%d}\n" i i (i * 10))
    done;

    let project = new Project_scan.t tbl_scan [ "A"; "C" ] in

    Buffer.add_string env.output "\nProjected records (A,C only):\n";
    project#before_first;
    while project#next do
      let a = Int32.to_int (project#get_int32 ~field_name:"A") in
      let c = Int32.to_int (project#get_int32 ~field_name:"C") in
      Buffer.add_string env.output (Printf.sprintf "{A=%d, C=%d}\n" a c)
    done;

    cleanup_test_env project env;
    let output = Buffer.contents env.output in
    write_test_output ~test_name:"project_scan" ~output
      ~db_name:"projectscan_test0";
    output

  (* Product Scan Test *)
  let test_product_scan0 () =
    let env = make_test_env ~db_name:"productscan_test0" in

    let schema1 = Schema.make () in
    Schema.add_int_field schema1 "A";
    Schema.add_string_field schema1 "B" 9;
    let layout1 = Layout.make schema1 in
    let tbl_scan1 =
      Table_scan.make ~tx:env.transaction ~tbl_name:"T1" ~layout:layout1
    in

    let schema2 = Schema.make () in
    Schema.add_int_field schema2 "C";
    Schema.add_string_field schema2 "D" 9;
    let layout2 = Layout.make schema2 in
    let tbl_scan2 =
      Table_scan.make ~tx:env.transaction ~tbl_name:"T2" ~layout:layout2
    in

    Buffer.add_string env.output "Inserting records into T1:\n";

    tbl_scan1#before_first;
    for i = 1 to 3 do
      tbl_scan1#insert;
      tbl_scan1#set_int32 ~field_name:"A" ~value:(Int32.of_int i);
      tbl_scan1#set_string ~field_name:"B" ~value:(Printf.sprintf "rec%d" i);
      Buffer.add_string env.output
        (Printf.sprintf "T1: Inserted {A=%d, B=rec%d}\n" i i)
    done;

    Buffer.add_string env.output "\nInserting records into T2:\n";

    tbl_scan2#before_first;
    for i = 1 to 2 do
      tbl_scan2#insert;
      tbl_scan2#set_int32 ~field_name:"C" ~value:(Int32.of_int (i * 10));
      tbl_scan2#set_string ~field_name:"D" ~value:(Printf.sprintf "val%d" i);
      Buffer.add_string env.output
        (Printf.sprintf "T2: Inserted {C=%d, D=val%d}\n" (i * 10) i)
    done;

    let lhs = Expression.make_field_name "A" in
    let rhs = Expression.make_const (Constant.Integer (Int32.of_int 2)) in
    let term = Term.make lhs rhs in
    let pred = Predicate.make term in
    let select = new Select_scan.t tbl_scan1 pred in

    Buffer.add_string env.output "\nSelect scan results (T1 where A=2):\n";
    select#before_first;
    while select#next do
      let a = Int32.to_int (select#get_int32 ~field_name:"A") in
      let b = select#get_string ~field_name:"B" in
      Buffer.add_string env.output
        (Printf.sprintf "Selected: {A=%d, B=%s}\n" a b)
    done;

    let product = new Product_scan.t select tbl_scan2 in

    Buffer.add_string env.output "\nProduct results (Selected T1 × T2):\n";
    product#before_first;
    while product#next do
      let a = Int32.to_int (product#get_int32 ~field_name:"A") in
      let b = product#get_string ~field_name:"B" in
      let c = Int32.to_int (product#get_int32 ~field_name:"C") in
      let d = product#get_string ~field_name:"D" in
      Buffer.add_string env.output
        (Printf.sprintf "{A=%d, B=%s, C=%d, D=%s}\n" a b c d)
    done;

    cleanup_test_env product env;
    let output = Buffer.contents env.output in
    write_test_output ~test_name:"product_scan" ~output
      ~db_name:"productscan_test0";
    output

  (* Select Select Test *)
  let test_select_select0 () =
    let env = make_test_env ~db_name:"selectselect_test0" in
    let schema = Schema.make () in
    Schema.add_int_field schema "A";
    Schema.add_int_field schema "B";
    Schema.add_string_field schema "C" 9;
    let layout = Layout.make schema in
    let tbl_scan = Table_scan.make ~tx:env.transaction ~tbl_name:"T" ~layout in

    Buffer.add_string env.output "Initial records:\n";

    tbl_scan#before_first;
    for i = 0 to 5 do
      tbl_scan#insert;
      tbl_scan#set_int32 ~field_name:"A" ~value:(Int32.of_int (i mod 3));
      tbl_scan#set_int32 ~field_name:"B" ~value:(Int32.of_int (i mod 2));
      tbl_scan#set_string ~field_name:"C"
        ~value:(Printf.sprintf "rec%d" (i + 1));
      Buffer.add_string env.output
        (Printf.sprintf "Inserted: {A=%d, B=%d, C=rec%d}\n" (i mod 3) (i mod 2)
           (i + 1))
    done;

    let lhs1 = Expression.make_field_name "A" in
    let rhs1 = Expression.make_const (Constant.Integer (Int32.of_int 2)) in
    let term1 = Term.make lhs1 rhs1 in
    let pred1 = Predicate.make term1 in
    let select1 = new Select_scan.t tbl_scan pred1 in

    Buffer.add_string env.output "\nFirst selection (A = 2):\n";
    select1#before_first;
    while select1#next do
      let a = Int32.to_int (select1#get_int32 ~field_name:"A") in
      let b = Int32.to_int (select1#get_int32 ~field_name:"B") in
      let c = select1#get_string ~field_name:"C" in
      Buffer.add_string env.output (Printf.sprintf "{A=%d, B=%d, C=%s}\n" a b c)
    done;

    let lhs2 = Expression.make_field_name "B" in
    let rhs2 = Expression.make_const (Constant.Integer (Int32.of_int 1)) in
    let term2 = Term.make lhs2 rhs2 in
    let pred2 = Predicate.make term2 in
    let select2 = new Select_scan.t select1 pred2 in

    Buffer.add_string env.output "\nSecond selection (A = 2 AND B = 1):\n";
    select2#before_first;
    while select2#next do
      let a = Int32.to_int (select2#get_int32 ~field_name:"A") in
      let b = Int32.to_int (select2#get_int32 ~field_name:"B") in
      let c = select2#get_string ~field_name:"C" in
      Buffer.add_string env.output (Printf.sprintf "{A=%d, B=%d, C=%s}\n" a b c)
    done;

    cleanup_test_env select2 env;
    let output = Buffer.contents env.output in
    write_test_output ~test_name:"select_select" ~output
      ~db_name:"selectselect_test0";
    output

  (* Product Project Test *)
  let test_product_project_scan0 () =
    let env = make_test_env ~db_name:"productprojectscan_test0" in

    let schema1 = Schema.make () in
    Schema.add_int_field schema1 "A";
    Schema.add_string_field schema1 "B" 9;
    Schema.add_int_field schema1 "C";
    let layout1 = Layout.make schema1 in
    let tbl_scan1 =
      Table_scan.make ~tx:env.transaction ~tbl_name:"T1" ~layout:layout1
    in

    let schema2 = Schema.make () in
    Schema.add_int_field schema2 "D";
    Schema.add_string_field schema2 "E" 9;
    Schema.add_int_field schema2 "F";
    let layout2 = Layout.make schema2 in
    let tbl_scan2 =
      Table_scan.make ~tx:env.transaction ~tbl_name:"T2" ~layout:layout2
    in

    Buffer.add_string env.output "Inserting records into T1:\n";

    tbl_scan1#before_first;
    for i = 1 to 3 do
      tbl_scan1#insert;
      tbl_scan1#set_int32 ~field_name:"A" ~value:(Int32.of_int i);
      tbl_scan1#set_string ~field_name:"B" ~value:(Printf.sprintf "rec%d" i);
      tbl_scan1#set_int32 ~field_name:"C" ~value:(Int32.of_int (i * 10));
      Buffer.add_string env.output
        (Printf.sprintf "T1: {A=%d, B=rec%d, C=%d}\n" i i (i * 10))
    done;

    Buffer.add_string env.output "\nInserting records into T2:\n";

    tbl_scan2#before_first;
    for i = 1 to 2 do
      tbl_scan2#insert;
      tbl_scan2#set_int32 ~field_name:"D" ~value:(Int32.of_int (i * 100));
      tbl_scan2#set_string ~field_name:"E" ~value:(Printf.sprintf "val%d" i);
      tbl_scan2#set_int32 ~field_name:"F" ~value:(Int32.of_int (i * 1000));
      Buffer.add_string env.output
        (Printf.sprintf "T2: {D=%d, E=val%d, F=%d}\n" (i * 100) i (i * 1000))
    done;

    let project1 = new Project_scan.t tbl_scan1 [ "A"; "C" ] in
    let project2 = new Project_scan.t tbl_scan2 [ "D"; "F" ] in

    Buffer.add_string env.output "\nProjected records from T1 (A,C only):\n";
    project1#before_first;
    while project1#next do
      let a = Int32.to_int (project1#get_int32 ~field_name:"A") in
      let c = Int32.to_int (project1#get_int32 ~field_name:"C") in
      Buffer.add_string env.output (Printf.sprintf "{A=%d, C=%d}\n" a c)
    done;

    Buffer.add_string env.output "\nProjected records from T2 (D,F only):\n";
    project2#before_first;
    while project2#next do
      let d = Int32.to_int (project2#get_int32 ~field_name:"D") in
      let f = Int32.to_int (project2#get_int32 ~field_name:"F") in
      Buffer.add_string env.output (Printf.sprintf "{D=%d, F=%d}\n" d f)
    done;

    let product = new Product_scan.t project1 project2 in

    Buffer.add_string env.output
      "\nProduct of projections (T1[A,C] × T2[D,F]):\n";
    product#before_first;
    while product#next do
      let a = Int32.to_int (product#get_int32 ~field_name:"A") in
      let c = Int32.to_int (product#get_int32 ~field_name:"C") in
      let d = Int32.to_int (product#get_int32 ~field_name:"D") in
      let f = Int32.to_int (product#get_int32 ~field_name:"F") in
      Buffer.add_string env.output
        (Printf.sprintf "{A=%d, C=%d, D=%d, F=%d}\n" a c d f)
    done;

    cleanup_test_env product env;
    let output = Buffer.contents env.output in
    write_test_output ~test_name:"product_project" ~output
      ~db_name:"productprojectscan_test0";
    output

  (* Select Update Test *)
  let test_select_update0 () =
    let env = make_test_env ~db_name:"selectupdate_test0" in

    (* Create table *)
    let schema = Schema.make () in
    Schema.add_int_field schema "A";
    Schema.add_string_field schema "B" 9;
    Schema.add_int_field schema "C";
    let layout = Layout.make schema in
    let tbl_scan = Table_scan.make ~tx:env.transaction ~tbl_name:"T" ~layout in

    Buffer.add_string env.output "Initial records:\n";

    (* Insert initial records *)
    tbl_scan#before_first;
    for i = 1 to 5 do
      tbl_scan#insert;
      tbl_scan#set_int32 ~field_name:"A" ~value:(Int32.of_int i);
      tbl_scan#set_string ~field_name:"B" ~value:(Printf.sprintf "rec%d" i);
      tbl_scan#set_int32 ~field_name:"C" ~value:(Int32.of_int (i * 10));
      Buffer.add_string env.output
        (Printf.sprintf "Inserted: {A=%d, B=rec%d, C=%d}\n" i i (i * 10))
    done;

    (* Create select scan for records where A = 3 *)
    let lhs = Expression.make_field_name "A" in
    let rhs = Expression.make_const (Constant.make_integer 3l) in
    let term = Term.make lhs rhs in
    let pred = Predicate.make term in
    let select = new Select_scan.t tbl_scan pred in

    Buffer.add_string env.output "\nUpdating records where A = 3:\n";

    (* Update selected records *)
    select#before_first;
    while select#next do
      let a = Int32.to_int (select#get_int32 ~field_name:"A") in
      let new_c = a * 100 in
      select#set_int32 ~field_name:"C" ~value:(Int32.of_int new_c);
      Buffer.add_string env.output
        (Printf.sprintf "Updated record {A=%d} with C=%d\n" a new_c)
    done;

    (* Print final table contents *)
    Buffer.add_string env.output "\nFinal table contents:\n";
    tbl_scan#before_first;
    while tbl_scan#next do
      let a = Int32.to_int (tbl_scan#get_int32 ~field_name:"A") in
      let b = tbl_scan#get_string ~field_name:"B" in
      let c = Int32.to_int (tbl_scan#get_int32 ~field_name:"C") in
      Buffer.add_string env.output (Printf.sprintf "{A=%d, B=%s, C=%d}\n" a b c)
    done;

    cleanup_test_env tbl_scan env;
    let output = Buffer.contents env.output in
    write_test_output ~test_name:"select_update" ~output
      ~db_name:"selectupdate_test0";
    output

  (* Project of Select Scan Test *)
  let test_project_of_select_scan () =
    let env = make_test_env ~db_name:"project_of_select_scan_test" in
    let schema = Schema.make () in
    Schema.add_int_field schema "A";
    Schema.add_string_field schema "B" 9;
    Schema.add_int_field schema "C";
    let layout = Layout.make schema in
    let tbl_scan = Table_scan.make ~tx:env.transaction ~tbl_name:"T" ~layout in

    Buffer.add_string env.output "Initial records:\n";

    tbl_scan#before_first;
    for i = 1 to 5 do
      tbl_scan#insert;
      tbl_scan#set_int32 ~field_name:"A" ~value:(Int32.of_int i);
      tbl_scan#set_string ~field_name:"B" ~value:(Printf.sprintf "rec%d" i);
      tbl_scan#set_int32 ~field_name:"C" ~value:(Int32.of_int (i * 10));
      Buffer.add_string env.output
        (Printf.sprintf "Inserted: {A=%d, B=rec%d, C=%d}\n" i i (i * 10))
    done;

    (* Perform a select scan to filter records where A = 3 *)
    let pred = make_predicate "A" 3 in
    let select_scan = new Select_scan.t tbl_scan pred in

    (* Perform a project scan on the result of the select scan to select fields A and C *)
    let project_scan = new Project_scan.t select_scan [ "A"; "C" ] in

    Buffer.add_string env.output
      "\nProjected records (A,C only) after select (A=3):\n";
    project_scan#before_first;
    while project_scan#next do
      let a = Int32.to_int (project_scan#get_int32 ~field_name:"A") in
      let c = Int32.to_int (project_scan#get_int32 ~field_name:"C") in
      Buffer.add_string env.output (Printf.sprintf "{A=%d, C=%d}\n" a c)
    done;

    cleanup_test_env project_scan env;
    let output = Buffer.contents env.output in
    write_test_output ~test_name:"project_of_select_scan" ~output
      ~db_name:"project_of_select_scan_test";
    output
end

(* Expected outputs from original test files *)
let table_scan_expected =
  "Initial records:\n\
   Inserted: {A=1, B=rec1}\n\
   Inserted: {A=2, B=rec2}\n\
   Inserted: {A=3, B=rec3}\n"

let select_scan_expected =
  "Initial records:\n\
   Inserted: {A=1, B=rec1}\n\
   Inserted: {A=2, B=rec2}\n\
   Inserted: {A=3, B=rec3}\n\
   Inserted: {A=4, B=rec4}\n\
   Inserted: {A=5, B=rec5}\n\n\
   All records before filtering:\n\
   {A=1, B=rec1}\n\
   {A=2, B=rec2}\n\
   {A=3, B=rec3}\n\
   {A=4, B=rec4}\n\
   {A=5, B=rec5}\n\n\
   Filtered records (A = 3):\n\
   {A=3, B=rec3}\n"

let project_scan_expected =
  "Initial records:\n\
   Inserted: {A=1, B=rec1, C=10}\n\
   Inserted: {A=2, B=rec2, C=20}\n\
   Inserted: {A=3, B=rec3, C=30}\n\
   Inserted: {A=4, B=rec4, C=40}\n\
   Inserted: {A=5, B=rec5, C=50}\n\n\
   Projected records (A,C only):\n\
   {A=1, C=10}\n\
   {A=2, C=20}\n\
   {A=3, C=30}\n\
   {A=4, C=40}\n\
   {A=5, C=50}\n"

let product_scan_expected =
  "Inserting records into T1:\n\
   T1: Inserted {A=1, B=rec1}\n\
   T1: Inserted {A=2, B=rec2}\n\
   T1: Inserted {A=3, B=rec3}\n\n\
   Inserting records into T2:\n\
   T2: Inserted {C=10, D=val1}\n\
   T2: Inserted {C=20, D=val2}\n\n\
   Select scan results (T1 where A=2):\n\
   Selected: {A=2, B=rec2}\n\n\
   Product results (Selected T1 × T2):\n\
   {A=2, B=rec2, C=10, D=val1}\n\
   {A=2, B=rec2, C=20, D=val2}\n"

let select_select_expected =
  "Initial records:\n\
   Inserted: {A=0, B=0, C=rec1}\n\
   Inserted: {A=1, B=1, C=rec2}\n\
   Inserted: {A=2, B=0, C=rec3}\n\
   Inserted: {A=0, B=1, C=rec4}\n\
   Inserted: {A=1, B=0, C=rec5}\n\
   Inserted: {A=2, B=1, C=rec6}\n\n\
   First selection (A = 2):\n\
   {A=2, B=0, C=rec3}\n\
   {A=2, B=1, C=rec6}\n\n\
   Second selection (A = 2 AND B = 1):\n\
   {A=2, B=1, C=rec6}\n"

let product_project_expected =
  "Inserting records into T1:\n\
   T1: {A=1, B=rec1, C=10}\n\
   T1: {A=2, B=rec2, C=20}\n\
   T1: {A=3, B=rec3, C=30}\n\n\
   Inserting records into T2:\n\
   T2: {D=100, E=val1, F=1000}\n\
   T2: {D=200, E=val2, F=2000}\n\n\
   Projected records from T1 (A,C only):\n\
   {A=1, C=10}\n\
   {A=2, C=20}\n\
   {A=3, C=30}\n\n\
   Projected records from T2 (D,F only):\n\
   {D=100, F=1000}\n\
   {D=200, F=2000}\n\n\
   Product of projections (T1[A,C] × T2[D,F]):\n\
   {A=1, C=10, D=100, F=1000}\n\
   {A=1, C=10, D=200, F=2000}\n\
   {A=2, C=20, D=100, F=1000}\n\
   {A=2, C=20, D=200, F=2000}\n\
   {A=3, C=30, D=100, F=1000}\n\
   {A=3, C=30, D=200, F=2000}\n"

let select_update_expected =
  "Initial records:\n\
   Inserted: {A=1, B=rec1, C=10}\n\
   Inserted: {A=2, B=rec2, C=20}\n\
   Inserted: {A=3, B=rec3, C=30}\n\
   Inserted: {A=4, B=rec4, C=40}\n\
   Inserted: {A=5, B=rec5, C=50}\n\n\
   Updating records where A = 3:\n\
   Updated record {A=3} with C=300\n\n\
   Final table contents:\n\
   {A=1, B=rec1, C=10}\n\
   {A=2, B=rec2, C=20}\n\
   {A=3, B=rec3, C=300}\n\
   {A=4, B=rec4, C=40}\n\
   {A=5, B=rec5, C=50}\n"

let project_of_select_scan_expected =
  "Initial records:\n\
   Inserted: {A=1, B=rec1, C=10}\n\
   Inserted: {A=2, B=rec2, C=20}\n\
   Inserted: {A=3, B=rec3, C=30}\n\
   Inserted: {A=4, B=rec4, C=40}\n\
   Inserted: {A=5, B=rec5, C=50}\n\n\
   Projected records (A,C only) after select (A=3):\n\
   {A=3, C=30}\n"

(* Test drivers *)
let test_table_scan () =
  Alcotest.(check string)
    "table scan" table_scan_expected
    (To_test.test_table_scan0 ())

let test_select_scan () =
  Alcotest.(check string)
    "select scan" select_scan_expected
    (To_test.test_select_scan0 ())

let test_project_scan () =
  Alcotest.(check string)
    "project scan" project_scan_expected
    (To_test.test_project_scan0 ())

let test_product_scan () =
  Alcotest.(check string)
    "product scan" product_scan_expected
    (To_test.test_product_scan0 ())

let test_select_select () =
  Alcotest.(check string)
    "select select" select_select_expected
    (To_test.test_select_select0 ())

let test_product_project () =
  Alcotest.(check string)
    "product project" product_project_expected
    (To_test.test_product_project_scan0 ())

let test_select_update () =
  Alcotest.(check string)
    "select update" select_update_expected
    (To_test.test_select_update0 ())

let test_project_of_select_scan () =
  Alcotest.(check string)
    "project of select scan" project_of_select_scan_expected
    (To_test.test_project_of_select_scan ())

(* All tests *)
let all_tests () =
  [
    Alcotest.test_case "table scan" `Quick test_table_scan;
    Alcotest.test_case "select scan" `Quick test_select_scan;
    Alcotest.test_case "project scan" `Quick test_project_scan;
    Alcotest.test_case "product scan" `Quick test_product_scan;
    Alcotest.test_case "select select" `Quick test_select_select;
    Alcotest.test_case "product project" `Quick test_product_project;
    Alcotest.test_case "select update" `Quick test_select_update;
    Alcotest.test_case "project of select scan" `Quick
      test_project_of_select_scan;
  ]
