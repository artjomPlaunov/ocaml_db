{
open Grammar
exception Lexing_error of string
}

let digit = ['0'-'9']
let int = digit+
let alpha = ['a'-'z' 'A'-'Z']
let id = alpha (alpha | digit | '_')*
let whitespace = [' ' '\t' '\n']

rule token = parse
  | whitespace    { token lexbuf }
  | "SELECT"      { SELECT }
  | "CREATE"      { CREATE }
  | "TABLE"       { TABLE }
  | "FROM"        { FROM }
  | "WHERE"       { WHERE }
  | "AND"         { AND }
  | "INSERT"      { INSERT }
  | "INTO"        { INTO }
  | "VALUES"      { VALUES }
  | "DELETE"      { DELETE }
  | "UPDATE"      { UPDATE }
  | "SET"         { SET }
  | "VIEW"        { VIEW }
  | "AS"          { AS }
  | "INDEX"       { INDEX }
  | "ON"          { ON }
  | "INT"         { INT_TYPE }
  | "VARCHAR"     { VARCHAR }
  | "="          { EQUALS }
  | ","          { COMMA }
  | "("          { LPAREN }
  | ")"          { RPAREN }
  | int as i     { INTEGER(int_of_string i) }
  | '"'          { read_string (Buffer.create 16) lexbuf }
  | id as word   { ID(word) }
  | eof          { EOF }
  | _            { raise (Lexing_error ("Unexpected char: " ^ Lexing.lexeme lexbuf)) }

and read_string buf = parse
  | '"'       { STRING (Buffer.contents buf) }
  | _ as c    { Buffer.add_char buf c; read_string buf lexbuf }
  | eof       { raise (Lexing_error ("String is not terminated")) } 