module To_test = struct
  open Parser

  let test_simple_select () =
    let query = "SELECT A, B FROM T1" in
    let lexbuf = Lexing.from_string query in
    let result = Parser.Grammar.prog Lexer.token lexbuf in
    match result with
    | Query_data.Select { fields; tables; predicate } ->
        Alcotest.(check (list string)) "fields" ["A"; "B"] fields;
        Alcotest.(check (list string)) "tables" ["T1"] tables;
        Alcotest.(check bool) "no predicate" true (predicate = None);
        "SELECT parsed successfully"
    | _ -> 
        Alcotest.fail "Expected Select query"

  let test_select_with_where () =
    let query = "SELECT A, B FROM T1 WHERE A = 2" in
    let lexbuf = Lexing.from_string query in
    let result = Parser.Grammar.prog Lexer.token lexbuf in
    match result with
    | Select { fields; tables; predicate = Some _ } ->
        Alcotest.(check (list string)) "fields" ["A"; "B"] fields;
        Alcotest.(check (list string)) "tables" ["T1"] tables;
        "SELECT WITH WHERE parsed successfully"
    | _ -> 
        Alcotest.fail "Expected Select query with WHERE clause"

  let test_create_table () =
    let query = "CREATE TABLE Students (id INT, name VARCHAR(50))" in
    let lexbuf = Lexing.from_string query in
    let result = Parser.Grammar.prog Lexer.token lexbuf in
    match result with
    | CreateTable { table_name; fields } ->
        Alcotest.(check string) "table name" "Students" table_name;
        Alcotest.(check int) "field count" 2 (List.length fields);
        Alcotest.(check string) "first field name" "id" (fst (List.hd fields));
        "CREATE TABLE parsed successfully"
    | _ -> 
        Alcotest.fail "Expected CreateTable query"
end

let test_simple_select () =
  Alcotest.(check string) "parse simple select" "SELECT parsed successfully" 
    (To_test.test_simple_select ())

let test_select_with_where () =
  Alcotest.(check string) "parse select with where" "SELECT WITH WHERE parsed successfully"
    (To_test.test_select_with_where ())

let test_create_table () =
  Alcotest.(check string) "parse create table" "CREATE TABLE parsed successfully"
    (To_test.test_create_table ())

let all_tests () =
  [
    Alcotest.test_case "simple select" `Quick test_simple_select;
    Alcotest.test_case "select with where" `Quick test_select_with_where;
    Alcotest.test_case "create table" `Quick test_create_table;
  ]