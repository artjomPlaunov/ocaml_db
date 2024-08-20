let () =
  let lexbuf = Lexing.from_channel stdin in
  try 
    let p = Parser.program Lexer.token lexbuf in
    List.iter (fun stmt -> Printf.printf "%s\n" (Ast.string_of_stmt stmt)) p
  with
  | Lexer.Error msg -> Stdio.printf "%s%!" msg
  | Parser.Error ->
    Stdio.printf "At offset %d: syntax error.\n%!" (Lexing.lexeme_start lexbuf)

    

