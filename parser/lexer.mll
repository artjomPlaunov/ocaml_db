{
open Grammar
exception Lexing_error of string
let keywords = [
  ("select", SELECT);
  ("create", CREATE);
  ("table", TABLE);
  ("from", FROM);
  ("where", WHERE);
  ("and", AND);
  ("insert", INSERT);
  ("into", INTO);
  ("values", VALUES);
  ("delete", DELETE);
  ("update", UPDATE);
  ("set", SET);
  ("view", VIEW);
  ("as", AS);
  ("index", INDEX);
  ("on", ON);
  ("int", INT_TYPE);
  ("varchar", VARCHAR)
] |> List.to_seq |> Hashtbl.of_seq
}

let digit = ['0'-'9']
let int = "-"?digit+
let alpha = ['a'-'z' 'A'-'Z']
let id = alpha (alpha | digit | '_')*
let whitespace = [' ' '\t' '\n']

rule token = parse
  | whitespace+   { token lexbuf }
  | id as word    {
      try
        Hashtbl.find keywords (String.lowercase_ascii word)
      with Not_found -> ID(word)
    }
  | "="          { EQUALS }
  | ","          { COMMA }
  | "("          { LPAREN }
  | ")"          { RPAREN }
  | int as i     { INTEGER(int_of_string i) }
  | '"'          { read_string (Buffer.create 16) lexbuf }
  | eof          { EOF }
  | _            { raise (Lexing_error ("Unexpected char: " ^ Lexing.lexeme lexbuf)) }

and read_string buf = parse
  | '"'       { STRING (Buffer.contents buf) }
  | _ as c    { Buffer.add_char buf c; read_string buf lexbuf }
  | eof       { raise (Lexing_error ("String is not terminated")) }
