{
  open Parser
  exception Error of string
}

let digit = ['0'-'9']
let int = digit+
let letter = ['a'-'z' 'A'-'Z']
let var = letter+
let ws = [' ' '\t' '\n' '\r']+

rule token = parse
  | "create"          { CREATE }
  | "table"           { TABLE }
  | "insert"          { INSERT }
  | "into"            { INTO }
  | "values"          { VALUES }
  | "select"          { SELECT }
  | "from"            { FROM }
  | "("                { LPAREN }
  | ")"                { RPAREN }
  | ","                { COMMA }
  | ";"                { SEMICOLON }
  | eof                 { EOF}
  | digit+             { INT (int_of_string (Lexing.lexeme lexbuf)) }
  | '"' ([^ '"'] | '\\' '"')* '"' { STRING (String.sub (Lexing.lexeme lexbuf) 1 (String.length (Lexing.lexeme lexbuf) - 2)) }
  | var { IDENT (Lexing.lexeme lexbuf) }
  | ws+         { token lexbuf } (* Skip whitespace *)
  | _                  { failwith "Unexpected character" }
