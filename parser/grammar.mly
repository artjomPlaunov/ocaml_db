%{
  open Query_data
  open Predicate
  open Constant
%}

%token <int> INTEGER
%token <string> ID
%token <string> STRING
%token SELECT CREATE TABLE FROM WHERE AND
%token INT_TYPE VARCHAR
%token EQUALS COMMA LPAREN RPAREN
%token EOF

%start <Query_data.query_data> prog

%%

prog:
  | q = query EOF { q }

query:
  | select_query { $1 }
  | create_table_query { $1 }

select_query:
  | SELECT fields = separated_list(COMMA, field) 
    FROM tables = separated_list(COMMA, ID)
    where_clause = option(preceded(WHERE, predicate))
    { Select { fields; tables; predicate = where_clause } }

create_table_query:
  | CREATE TABLE name = ID 
    LPAREN fields = separated_list(COMMA, field_def) RPAREN
    { CreateTable { table_name = name; fields = fields } }

field:
  | id = ID { id }

field_def:
  | name = ID INT_TYPE { (name, `Int) }
  | name = ID VARCHAR LPAREN size = INTEGER RPAREN { (name, `String size) }

predicate:
  | t = term { Predicate.make t }

term:
  | lhs = expr EQUALS rhs = expr 
    { Predicate__Term.make lhs rhs }

expr:
  | id = ID { Predicate__Expression.make_field_name id }
  | i = INTEGER { Predicate__Expression.make_const (Constant.Integer (Int32.of_int i)) }
  | s = STRING { Predicate__Expression.make_const (Constant.String s) }

%% 