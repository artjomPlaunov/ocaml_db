module To_test = struct
  open File
  open Test_utils
  module Schema = Record_page__Schema
  module Layout = Record_page__Layout
  module Expression = Predicate__Expression
  module Term = Predicate__Term
  module Constant = Constant

  let test_project_scan0 () =
    let env = make_test_env ~db_name:"projectscan_test0" in
    
    (* Create table *)
    let schema = Schema.make () in
    Schema.add_int_field schema "A";
    Schema.add_string_field schema "B" 9;
    Schema.add_int_field schema "C";
    let layout = Layout.make schema in
    let tbl_scan = Table_scan.make ~tx:env.transaction ~tbl_name:"T" ~layout in
    
    Buffer.add_string env.output "Initial records:\n";
    
    (* Insert records *)
    tbl_scan#before_first;
    for i = 1 to 5 do
      tbl_scan#insert;
      tbl_scan#set_int32 ~field_name:"A" ~value:(Int32.of_int i);
      tbl_scan#set_string ~field_name:"B" ~value:(Printf.sprintf "rec%d" i);
      tbl_scan#set_int32 ~field_name:"C" ~value:(Int32.of_int (i * 10));
      Buffer.add_string env.output 
        (Printf.sprintf "Inserted: {A=%d, B=rec%d, C=%d}\n" i i (i * 10))
    done;

    (* Print all records before projection *)
    Buffer.add_string env.output "\nAll records before projection:\n";
    tbl_scan#before_first;
    while tbl_scan#next do
      let a = Int32.to_int (tbl_scan#get_int32 ~field_name:"A") in
      let b = tbl_scan#get_string ~field_name:"B" in
      let c = Int32.to_int (tbl_scan#get_int32 ~field_name:"C") in
      Buffer.add_string env.output 
        (Printf.sprintf "{A=%d, B=%s, C=%d}\n" a b c)
    done;
    
    (* Create project scan with fields A and C *)
    let project = new Project_scan.t tbl_scan ["A"; "C"] in
    
    (* Print projected records *)
    Buffer.add_string env.output "\nProjected records (A, C only):\n";
    project#before_first;
    while project#next do
      let a = Int32.to_int (project#get_int32 ~field_name:"A") in
      let c = Int32.to_int (project#get_int32 ~field_name:"C") in
      Buffer.add_string env.output 
        (Printf.sprintf "{A=%d, C=%d}\n" a c)
    done;
    
    (* Cleanup *)
    cleanup_test_env project env;
    Buffer.contents env.output
end 

let expected_output = "Initial records:
Inserted: {A=1, B=rec1, C=10}
Inserted: {A=2, B=rec2, C=20}
Inserted: {A=3, B=rec3, C=30}
Inserted: {A=4, B=rec4, C=40}
Inserted: {A=5, B=rec5, C=50}

All records before projection:
{A=1, B=rec1, C=10}
{A=2, B=rec2, C=20}
{A=3, B=rec3, C=30}
{A=4, B=rec4, C=40}
{A=5, B=rec5, C=50}

Projected records (A, C only):
{A=1, C=10}
{A=2, C=20}
{A=3, C=30}
{A=4, C=40}
{A=5, C=50}
"

let test_project_scan0 () =
  Alcotest.(check string) "same output" expected_output (To_test.test_project_scan0 ())

let all_tests () =
  [ Alcotest.test_case "project scan basic ops" `Quick test_project_scan0 ]