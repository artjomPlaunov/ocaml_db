module To_test = struct
  open Parser

  let test_simple_select () =
    let query = "SELECT A, B FROM T1" in
    let lexbuf = Lexing.from_string query in
    let result = Parser.Grammar.prog Lexer.token lexbuf in
    match result with
    | Query_data.Select { fields; tables; predicate } ->
        Alcotest.(check (list string)) "fields" [ "A"; "B" ] fields;
        Alcotest.(check (list string)) "tables" [ "T1" ] tables;
        Alcotest.(check bool) "no predicate" true (predicate = None);
        "SELECT parsed successfully"
    | _ -> Alcotest.fail "Expected Select query"

  let test_select_with_where () =
    let query = "SELECT A, B, C FROM T1 WHERE A = 2" in
    let lexbuf = Lexing.from_string query in
    let result = Parser.Grammar.prog Lexer.token lexbuf in
    match result with
    | Query_data.Select { fields; tables; predicate = Some _ } ->
        Alcotest.(check (list string)) "fields" [ "A"; "B"; "C" ] fields;
        Alcotest.(check (list string)) "tables" [ "T1" ] tables;
        "SELECT WITH WHERE parsed successfully"
    | _ -> Alcotest.fail "Expected Select query with WHERE clause"

  let test_create_table () =
    let query = "CREATE TABLE Students (id INT, name VARCHAR(50))" in
    let lexbuf = Lexing.from_string query in
    let result = Parser.Grammar.prog Lexer.token lexbuf in
    match result with
    | Query_data.CreateTable { tblname; sch } ->
        Alcotest.(check string) "table name" "Students" tblname;
        Alcotest.(check bool)
          "schema has field 'id'" true
          (Record_page.Schema.has_field sch "id");
        Alcotest.(check bool)
          "schema has field 'name'" true
          (Record_page.Schema.has_field sch "name");
        Alcotest.(check bool)
          "field 'id' is INT" true
          (match Record_page.Schema.get_type sch "id" with
          | Record_page.Type.Integer -> true
          | _ -> false);
        Alcotest.(check bool)
          "field 'name' is VARCHAR(50)" true
          (match Record_page.Schema.get_type sch "name" with
          | Record_page.Type.Varchar ->
              Record_page.Schema.get_length sch "name" = 50
          | _ -> false);
        "CREATE TABLE parsed successfully"
    | _ -> Alcotest.fail "Expected CreateTable query"

  let create_table_query tblname fields =
    let fields_str = String.concat ", " fields in
    Printf.sprintf "CREATE TABLE %s (%s)" tblname fields_str

  let check_field schema field_name expected_type expected_length =
    Alcotest.(check bool)
      (Printf.sprintf "schema has field '%s'" field_name)
      true
      (Record_page.Schema.has_field schema field_name);
    Alcotest.(check bool)
      (Printf.sprintf "field '%s' is %s" field_name expected_type)
      true
      (match Record_page.Schema.get_type schema field_name with
      | Record_page.Type.Integer when expected_type = "INT" -> true
      | Record_page.Type.Varchar when expected_type = "VARCHAR" ->
          Record_page.Schema.get_length schema field_name = expected_length
      | _ -> false)

  let test_create_table_large () =
    let fields =
      [
        "id INT";
        "name VARCHAR(50)";
        "age INT";
        "email VARCHAR(100)";
        "phone VARCHAR(15)";
        "address VARCHAR(200)";
        "city VARCHAR(50)";
        "state VARCHAR(20)";
        "zip INT";
        "country VARCHAR(50)";
      ]
    in
    let query = create_table_query "LargeTable" fields in
    let lexbuf = Lexing.from_string query in
    let result = Parser.Grammar.prog Lexer.token lexbuf in
    match result with
    | Query_data.CreateTable { tblname; sch } ->
        Alcotest.(check string) "table name" "LargeTable" tblname;
        check_field sch "id" "INT" 0;
        check_field sch "name" "VARCHAR" 50;
        check_field sch "age" "INT" 0;
        check_field sch "email" "VARCHAR" 100;
        check_field sch "phone" "VARCHAR" 15;
        check_field sch "address" "VARCHAR" 200;
        check_field sch "city" "VARCHAR" 50;
        check_field sch "state" "VARCHAR" 20;
        check_field sch "zip" "INT" 0;
        check_field sch "country" "VARCHAR" 50;
        "CREATE TABLE with many fields parsed successfully"
    | _ -> Alcotest.fail "Expected CreateTable query"

  let test_insert () =
    let query = "INSERT INTO Students (id) VALUES (1)" in
    let lexbuf = Lexing.from_string query in
    try
      let result = Parser.Grammar.prog Lexer.token lexbuf in
      match result with
      | Query_data.Insert { tblname; flds; vals } ->
          Alcotest.(check string) "table name" "Students" tblname;
          Alcotest.(check (list string)) "fields" [ "id" ] flds;
          Alcotest.(check int) "values count" 1 (List.length vals);
          "INSERT parsed successfully"
      | _ -> Alcotest.fail "Expected Insert query"
    with
    | Lexer.Lexing_error msg ->
        Printf.eprintf "Lexing error: %s\n" msg;
        Alcotest.fail "Lexing error encountered"
    | Parsing.Parse_error ->
        Printf.eprintf "Parsing error at position: %d\n"
          (Lexing.lexeme_start lexbuf);
        Alcotest.fail "Parsing error encountered"
    | e ->
        Printf.eprintf "Unexpected error: %s\n" (Printexc.to_string e);
        Alcotest.fail "Unexpected error encountered"

  let test_delete () =
    let query = "DELETE FROM Students WHERE id = 1" in
    let lexbuf = Lexing.from_string query in
    let result = Parser.Grammar.prog Lexer.token lexbuf in
    match result with
    | Query_data.Delete { tblname; pred = _ } ->
        Alcotest.(check string) "table name" "Students" tblname;
        "DELETE parsed successfully"
    | _ -> Alcotest.fail "Expected Delete query"

  let test_modify () =
    let query = "UPDATE Students SET name = \"Doe\" WHERE id = 1" in
    let lexbuf = Lexing.from_string query in
    let result = Parser.Grammar.prog Lexer.token lexbuf in
    match result with
    | Query_data.Modify { tblname; fldname; newval; pred = _ } ->
        Alcotest.(check string) "table name" "Students" tblname;
        Alcotest.(check string) "field name" "name" fldname;
        "MODIFY parsed successfully"
    | _ -> Alcotest.fail "Expected Modify query"

  let test_create_view () =
    let query = "CREATE VIEW StudentView AS SELECT id FROM Students" in
    let lexbuf = Lexing.from_string query in
    let result = Parser.Grammar.prog Lexer.token lexbuf in
    match result with
    | Query_data.CreateView { viewname; qrydata = Query_data.Select _ } ->
        Alcotest.(check string) "view name" "StudentView" viewname;
        "CREATE VIEW parsed successfully"
    | _ -> Alcotest.fail "Expected CreateView query"

  let test_create_index () =
    let query = "CREATE INDEX idx_name ON table_name (field_name)" in
    let lexbuf = Lexing.from_string query in
    let result = Parser.Grammar.prog Lexer.token lexbuf in
    match result with
    | Query_data.CreateIndex { idxname; tblname; fldname } ->
        Alcotest.(check string) "index name" "idx_name" idxname;
        Alcotest.(check string) "table name" "table_name" tblname;
        Alcotest.(check string) "field name" "field_name" fldname;
        "CREATE INDEX parsed successfully"
    | _ -> Alcotest.fail "Expected CreateIndex query"

  let test_insert_multiple_fields () =
    let query =
      "INSERT INTO Students (id, name, age) VALUES (1, \"John Doe\", 20)"
    in
    let lexbuf = Lexing.from_string query in
    try
      let result = Parser.Grammar.prog Lexer.token lexbuf in
      match result with
      | Query_data.Insert { tblname; flds; vals } ->
          Alcotest.(check string) "table name" "Students" tblname;
          Alcotest.(check (list string)) "fields" [ "id"; "name"; "age" ] flds;
          Alcotest.(check int) "values count" 3 (List.length vals);
          "INSERT with multiple fields parsed successfully"
      | _ -> Alcotest.fail "Expected Insert query"
    with
    | Lexer.Lexing_error msg ->
        Printf.eprintf "Lexing error: %s\n" msg;
        Alcotest.fail "Lexing error encountered"
    | Parsing.Parse_error ->
        Printf.eprintf "Parsing error at position: %d\n"
          (Lexing.lexeme_start lexbuf);
        Alcotest.fail "Parsing error encountered"
    | e ->
        Printf.eprintf "Unexpected error: %s\n" (Printexc.to_string e);
        Alcotest.fail "Unexpected error encountered"
end

let test_simple_select () =
  Alcotest.(check string)
    "parse simple select" "SELECT parsed successfully"
    (To_test.test_simple_select ())

let test_select_with_where () =
  Alcotest.(check string)
    "parse select with where" "SELECT WITH WHERE parsed successfully"
    (To_test.test_select_with_where ())

let test_create_table () =
  Alcotest.(check string)
    "parse create table" "CREATE TABLE parsed successfully"
    (To_test.test_create_table ())

let test_create_table_large () =
  Alcotest.(check string)
    "parse create table with many fields"
    "CREATE TABLE with many fields parsed successfully"
    (To_test.test_create_table_large ())

let test_insert () =
  Alcotest.(check string)
    "parse insert" "INSERT parsed successfully" (To_test.test_insert ())

let test_delete () =
  Alcotest.(check string)
    "parse delete" "DELETE parsed successfully" (To_test.test_delete ())

let test_modify () =
  Alcotest.(check string)
    "parse modify" "MODIFY parsed successfully" (To_test.test_modify ())

let test_create_view () =
  Alcotest.(check string)
    "parse create view" "CREATE VIEW parsed successfully"
    (To_test.test_create_view ())

let test_create_index () =
  Alcotest.(check string)
    "parse create index" "CREATE INDEX parsed successfully"
    (To_test.test_create_index ())

let test_insert_multiple_fields () =
  Alcotest.(check string)
    "parse insert with multiple fields"
    "INSERT with multiple fields parsed successfully"
    (To_test.test_insert_multiple_fields ())

let all_tests () =
  [
    Alcotest.test_case "simple select" `Quick test_simple_select;
    Alcotest.test_case "select with where" `Quick test_select_with_where;
    Alcotest.test_case "create table" `Quick test_create_table;
    Alcotest.test_case "create table with many fields" `Quick
      test_create_table_large;
    Alcotest.test_case "insert" `Quick test_insert;
    Alcotest.test_case "delete" `Quick test_delete;
    Alcotest.test_case "modify" `Quick test_modify;
    Alcotest.test_case "create view" `Quick test_create_view;
    Alcotest.test_case "create index" `Quick test_create_index;
    Alcotest.test_case "insert with multiple fields" `Quick
      test_insert_multiple_fields;
  ]
