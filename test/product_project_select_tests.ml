module To_test = struct
  open File
  open Test_utils
  module Schema = Record_page__Schema
  module Layout = Record_page__Layout
  module Expression = Predicate__Expression
  module Term = Predicate__Term
  module Constant = Constant

  let test_product_project_scan0 () =
    let env = make_test_env ~db_name:"productprojectscan_test0" in
    
    (* Create first table *)
    let schema1 = Schema.make () in
    Schema.add_int_field schema1 "A";
    Schema.add_string_field schema1 "B" 9;
    Schema.add_int_field schema1 "C";
    let layout1 = Layout.make schema1 in
    let tbl_scan1 = Table_scan.make ~tx:env.transaction ~tbl_name:"T1" ~layout:layout1 in
    
    (* Create second table *)
    let schema2 = Schema.make () in
    Schema.add_int_field schema2 "D";
    Schema.add_string_field schema2 "E" 9;
    Schema.add_int_field schema2 "F";
    let layout2 = Layout.make schema2 in
    let tbl_scan2 = Table_scan.make ~tx:env.transaction ~tbl_name:"T2" ~layout:layout2 in
    
    Buffer.add_string env.output "Inserting records into T1:\n";
    
    (* Insert records into T1 *)
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
    
    (* Insert records into T2 *)
    tbl_scan2#before_first;
    for i = 1 to 2 do
      tbl_scan2#insert;
      tbl_scan2#set_int32 ~field_name:"D" ~value:(Int32.of_int (i * 100));
      tbl_scan2#set_string ~field_name:"E" ~value:(Printf.sprintf "val%d" i);
      tbl_scan2#set_int32 ~field_name:"F" ~value:(Int32.of_int (i * 1000));
      Buffer.add_string env.output 
        (Printf.sprintf "T2: {D=%d, E=val%d, F=%d}\n" (i * 100) i (i * 1000))
    done;

    (* Create project scans *)
    let project1 = new Project_scan.t tbl_scan1 ["A"; "C"] in
    let project2 = new Project_scan.t tbl_scan2 ["D"; "F"] in
    
    (* Print projected records from T1 *)
    Buffer.add_string env.output "\nProjected records from T1 (A,C only):\n";
    project1#before_first;
    while project1#next do
      let a = Int32.to_int (project1#get_int32 ~field_name:"A") in
      let c = Int32.to_int (project1#get_int32 ~field_name:"C") in
      Buffer.add_string env.output 
        (Printf.sprintf "{A=%d, C=%d}\n" a c)
    done;

    (* Print projected records from T2 *)
    Buffer.add_string env.output "\nProjected records from T2 (D,F only):\n";
    project2#before_first;
    while project2#next do
      let d = Int32.to_int (project2#get_int32 ~field_name:"D") in
      let f = Int32.to_int (project2#get_int32 ~field_name:"F") in
      Buffer.add_string env.output 
        (Printf.sprintf "{D=%d, F=%d}\n" d f)
    done;

    (* Create product scan of the two projections *)
    let product = new Product_scan.t project1 project2 in
    
    (* Print product results *)
    Buffer.add_string env.output "\nProduct of projections (T1[A,C] × T2[D,F]):\n";
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
    Buffer.contents env.output
end

let expected_output = "Inserting records into T1:
T1: {A=1, B=rec1, C=10}
T1: {A=2, B=rec2, C=20}
T1: {A=3, B=rec3, C=30}

Inserting records into T2:
T2: {D=100, E=val1, F=1000}
T2: {D=200, E=val2, F=2000}

Projected records from T1 (A,C only):
{A=1, C=10}
{A=2, C=20}
{A=3, C=30}

Projected records from T2 (D,F only):
{D=100, F=1000}
{D=200, F=2000}

Product of projections (T1[A,C] × T2[D,F]):
{A=1, C=10, D=100, F=1000}
{A=1, C=10, D=200, F=2000}
{A=2, C=20, D=100, F=1000}
{A=2, C=20, D=200, F=2000}
{A=3, C=30, D=100, F=1000}
{A=3, C=30, D=200, F=2000}
"

let test_product_project_scan0 () =
  Alcotest.(check string) "same output" expected_output (To_test.test_product_project_scan0 ())

let all_tests () =
  [ Alcotest.test_case "product of projections" `Quick test_product_project_scan0 ] 