module To_test = struct
  open File
  open Test_utils
  module Schema = Record_page__Schema
  module Layout = Record_page__Layout

  let test_select_scan0 () =
    let env = make_test_env ~db_name:"selectscan_test0" in
    
    (* Create table *)
    let schema = Schema.make () in
    Schema.add_int_field schema "A";
    Schema.add_string_field schema "B" 9;
    let layout = Layout.make schema in
    let tbl_scan = Table_scan.make ~tx:env.transaction ~tbl_name:"T" ~layout in
    
    Buffer.add_string env.output "Initial records:\n";
    
    (* Insert records *)
    tbl_scan#before_first;
    for i = 1 to 5 do
      tbl_scan#insert;
      tbl_scan#set_int32 ~field_name:"A" ~value:(Int32.of_int i);
      tbl_scan#set_string ~field_name:"B" ~value:(Printf.sprintf "rec%d" i);
      Buffer.add_string env.output (Printf.sprintf "Inserted: {A=%d, B=rec%d}\n" i i)
    done;

    (* Print all records *)
    let get_fields scan =
      let a = Int32.to_int (scan#get_int32 ~field_name:"A") in
      let b = scan#get_string ~field_name:"B" in
      Printf.sprintf "{A=%d, B=%s}\n" a b
    in
    print_table_contents ~output:env.output ~name:"All records before filtering" 
      ~scan:tbl_scan ~get_fields;

    (* Create and test selection *)
    let pred = make_predicate "A" 3 in
    let select = new Select_scan.t tbl_scan pred in
    
    print_table_contents ~output:env.output ~name:"Filtered records (A = 3)" 
      ~scan:select ~get_fields;
    
    cleanup_test_env select env;
    Buffer.contents env.output
end

let expected_output = "Initial records:
Inserted: {A=1, B=rec1}
Inserted: {A=2, B=rec2}
Inserted: {A=3, B=rec3}
Inserted: {A=4, B=rec4}
Inserted: {A=5, B=rec5}

All records before filtering:
{A=1, B=rec1}
{A=2, B=rec2}
{A=3, B=rec3}
{A=4, B=rec4}
{A=5, B=rec5}

Filtered records (A = 3):
{A=3, B=rec3}
"

let test_select_scan0 () =
  Alcotest.(check string) "same output" expected_output (To_test.test_select_scan0 ())

let all_tests () =
  [ Alcotest.test_case "select scan equality" `Quick test_select_scan0 ]