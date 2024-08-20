%{
  open Ast
%}

%token <int> INT
%token <string> STRING
%token <string> IDENT
%token CREATE TABLE INSERT INTO VALUES SELECT FROM
%token LPAREN RPAREN COMMA SEMICOLON EOF
%start <Ast.stmt list> program
%type <Ast.stmt> stmt
%type <Ast.stmt> create_table
%type <Ast.stmt> insert
%type <Ast.stmt> select

%%

program:
  stmts EOF                     { $1 }

stmts:
  stmt                       { [$1] }
  | stmts stmt                { $1 @ [$2] }

stmt:
  create_table               { $1 }
  | insert                   { $1 }
  | select                   { $1 }

create_table:
  CREATE TABLE IDENT LPAREN column_def_list RPAREN SEMICOLON { CREATE_TABLE ($3, $5) }

column_def_list:
  column_def                 { [$1] }
  | column_def_list COMMA column_def { $1 @ [$3] }

column_def:
  IDENT IDENT                 { ($1, $2) }

insert:
  INSERT INTO IDENT VALUES LPAREN value_list RPAREN SEMICOLON { INSERT ($3, $6) }

value_list:
  value                      { [$1] }
  | value_list COMMA value    { $1 @ [$3] }

value:
  INT                        { INT_VAL $1 }
  | STRING                   { STRING_VAL $1 }

select:
  SELECT IDENT FROM IDENT SEMICOLON { SELECT ($2, $4) }
