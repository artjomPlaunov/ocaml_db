open Parser
open File
open Ast

let db_name = "repl_db"

let make_env () =
  let file_manager = File_manager.make ~db_dirname:db_name ~block_size:1024 in
  let log_manager = Log_manager.make ~file_manager ~log_file:(db_name ^ "_logs") in
  let buffer_manager = Buffer_manager.make ~file_manager ~log_manager ~num_buffers:8 () in
  let tx = Transaction.make ~file_manager ~log_manager ~buffer_manager in
  Execution.make ~tx

let print_prompt () =
  print_string "sql> ";
  flush stdout

let execute_query e query =
  let lexbuf = Lexing.from_string query in
  try
    let ast = Parser.Grammar.prog Lexer.token lexbuf in
    let output = Buffer.create 256 in
    let _ = Execution.execute ~e ~query:ast ?output:(Some output) () in
    match ast with
    | Query.Select _ ->
        print_string (Buffer.contents output)
    | Query.Insert _ ->
        print_endline "Insert operation completed successfully."
    | Query.Delete _ ->
        print_endline "Delete operation completed successfully."
    | Query.Update _ ->
        print_endline "Update operation completed successfully."
    | Query.CreateTable _ ->
        print_endline "Table created successfully."
    | Query.CreateIndex _ ->
        print_endline "Index creation not yet implemented."
  with
  | Parsing.Parse_error -> print_endline "Syntax error in SQL query."
  | Failure msg -> print_endline ("Error: " ^ msg)
  | _ -> print_endline "An unexpected error occurred."

let rec repl_loop executor =
  print_prompt ();
  match read_line () with
  | exception End_of_file -> ()
  | "exit" | "quit" -> ()
  | query ->
      execute_query executor query;
      repl_loop executor

let () =
  print_endline "Welcome to SimpleDB SQL REPL";
  print_endline "Type 'exit' or 'quit' to end the session";
  print_endline "";
  let e = make_env () in
  repl_loop e
