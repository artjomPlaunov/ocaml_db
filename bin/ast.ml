type column_def = string * string
type value = INT_VAL of int | STRING_VAL of string

type stmt =
  | CREATE_TABLE of string * column_def list
  | INSERT of string * value list
  | SELECT of string * string

let string_of_stmt = function
  | CREATE_TABLE (table, cols) ->
      let col_defs = List.map (fun (name, typ) -> name ^ " " ^ typ) cols in
      Printf.sprintf "CREATE TABLE %s (%s)" table (String.concat ", " col_defs)
  | INSERT (table, values) ->
      let values_str = List.map (function INT_VAL i -> string_of_int i | STRING_VAL s -> "\"" ^ s ^ "\"") values in
      Printf.sprintf "INSERT INTO %s VALUES (%s)" table (String.concat ", " values_str)
  | SELECT (cols, table) ->
      Printf.sprintf "SELECT %s FROM %s" cols table
