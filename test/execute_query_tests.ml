open Parser
open File
open Test_utils

module To_test = struct
  let cleanup_test_env env =
    Transaction.commit env.transaction

  let setup_simple_select_data () =
    let env = Test_utils.make_test_env ~db_name:"simple_select_test" in
    let table_mgr = Table_manager.make ~is_new:true ~tx:env.transaction in
    let schema = Record_page__Schema.make () in
    Record_page__Schema.add_int_field schema "A";
    Record_page__Schema.add_string_field schema "B" 9;
    Table_manager.create_table ~table_mgr ~tbl_name:"T1" ~schema ~tx:env.transaction;
    let layout = Table_manager.get_layout ~table_mgr ~tbl_name:"T1" ~tx:env.transaction in
    let tbl_scan = Table_scan.make ~tx:env.transaction ~tbl_name:"T1" ~layout in

    tbl_scan#before_first;
    for i = 1 to 3 do
      tbl_scan#insert;
      tbl_scan#set_int32 ~field_name:"A" ~value:(Int32.of_int i);
      tbl_scan#set_string ~field_name:"B" ~value:(Printf.sprintf "rec%d" i)
    done;
    tbl_scan#close;
    Transaction.commit env.transaction

  let execute_simple_select_query () =
    let env = Test_utils.make_test_env ~db_name:"simple_select_test" in
    let interpreter_env = Interpreter.make_env 
      ~tx:env.transaction 
      ~buffer_manager:env.buffer_manager in
    let interpreter = new Interpreter.sql_interpreter interpreter_env in

    let query = "SELECT A, B FROM T1" in
    let lexbuf = Lexing.from_string query in
    let ast = Parser.Grammar.prog Lexer.token lexbuf in
    let output = Buffer.create 256 in
    interpreter#execute ~output ast;
    cleanup_test_env env;
    let filename = Test_utils.write_query_output 
      ~db_name:"simple_select_test" 
      ~query_name:"simple_select" 
      ~output:(Buffer.contents output) in
    let ch = open_in filename in
    let contents = really_input_string ch (in_channel_length ch) in
    close_in ch;
    contents

  let test_simple_select () =
    setup_simple_select_data ();
    execute_simple_select_query ()

  let setup_insert_data () =
    let env = Test_utils.make_test_env ~db_name:"insert_select_test" in
    let table_mgr = Table_manager.make ~is_new:true ~tx:env.transaction in
    let schema = Record_page__Schema.make () in
    Record_page__Schema.add_int_field schema "A";
    Record_page__Schema.add_string_field schema "B" 9;
    Table_manager.create_table ~table_mgr ~tbl_name:"T1" ~schema ~tx:env.transaction;
    let layout = Table_manager.get_layout ~table_mgr ~tbl_name:"T1" ~tx:env.transaction in
    let tbl_scan = Table_scan.make ~tx:env.transaction ~tbl_name:"T1" ~layout in
    tbl_scan#close;
    Transaction.commit env.transaction

  let execute_insert_and_select_query () =
    let env = Test_utils.make_test_env ~db_name:"insert_select_test" in
    let interpreter_env = Interpreter.make_env 
      ~tx:env.transaction 
      ~buffer_manager:env.buffer_manager in
    let interpreter = new Interpreter.sql_interpreter interpreter_env in

    let insert_query = "INSERT INTO T1 (A, B) VALUES (42, \"test\")" in
    let lexbuf = Lexing.from_string insert_query in
    let ast = Parser.Grammar.prog Lexer.token lexbuf in
    interpreter#execute ast;

    let select_query = "SELECT A, B FROM T1 WHERE A = 42" in
    let lexbuf = Lexing.from_string select_query in
    let ast = Parser.Grammar.prog Lexer.token lexbuf in
    let output = Buffer.create 256 in
    interpreter#execute ~output ast;
    cleanup_test_env env;
    let filepath = Test_utils.write_query_output 
      ~db_name:"insert_select_test" 
      ~query_name:"insert_and_select" 
      ~output:(Buffer.contents output) in
    let ch = open_in filepath in
    let contents = really_input_string ch (in_channel_length ch) in
    close_in ch;
    contents

  let test_insert_and_select () =
    setup_insert_data ();
    execute_insert_and_select_query ()
end

(* Expected outputs *)
let simple_select_expected = "Table: T1\n---------------+---------------\n| A               | B               |\n---------------+---------------\n| 1               | rec1            |\n| 2               | rec2            |\n| 3               | rec3            |\n---------------+---------------\n"
let insert_select_expected = "Table: T1\n---------------+---------------\n| A               | B               |\n---------------+---------------\n| 42              | test            |\n---------------+---------------\n"

(* Test suite *)
let all_tests () =
  [
    Alcotest.test_case "simple select" `Quick (fun () ->
      Alcotest.(check string) "simple select results" 
        simple_select_expected (To_test.test_simple_select ()));
    Alcotest.test_case "insert and select" `Quick (fun () ->
      Alcotest.(check string) "insert and select results" 
        insert_select_expected (To_test.test_insert_and_select ()));
  ]
