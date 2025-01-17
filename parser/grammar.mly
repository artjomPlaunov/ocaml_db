%{
  open Ast
  open Scans
  open Constant

  type local_type = 
    | LocalInteger
    | LocalVarchar of int
%}

%token <int> INTEGER
%token <string> ID
%token <string> STRING
%token SELECT CREATE TABLE FROM WHERE AND INSERT INTO VALUES DELETE UPDATE SET VIEW AS INDEX ON
%token INT_TYPE VARCHAR
%token EQUALS COMMA LPAREN RPAREN
%token EOF

%start <Query.t> prog

%%

prog:
    query EOF { $1 }
  | create EOF { $1 }
  ;

field:
  | ID { $1 }
  ;

constant:
  | STRING { Constant.make_string $1 }
  | INTEGER { Constant.make_integer (Int32.of_int $1) }
  ;

expression:
  | field { Scans__Expression.make_field_name $1 }
  | constant { Scans__Expression.make_const $1 }
  ;

term:
  | expression EQUALS expression { Scans__Term.make $1 $3 }
  ;

predicate:
  | term { Scans__Predicate.make $1 }
  (* | term AND predicate { Predicate.and_ $1 $3 } *)
  ;

query:
  | SELECT select_list FROM table_list WHERE predicate { 
      Query.Select (Select.make $2 $4 (Some $6))
    }
  | SELECT select_list FROM table_list { 
      Query.Select (Select.make $2 $4 None)
    }
  ;

select_list:
  | field { [$1] }
  | field COMMA select_list { $1 :: $3 }
  ;

table_list:
  | ID { [$1] }
  | ID COMMA table_list { $1 :: $3 }
  ;

create:
  | insert { $1 }
  | delete { $1 }
  | modify { $1 }
  | create_table { $1 }
  | create_index { $1 }
  ;

insert:
  | INSERT INTO ID LPAREN field_list RPAREN VALUES LPAREN const_list RPAREN {
      Query.Insert (Insert.make $3 $5 $9)
    }
  ;

field_list:
  | field { [$1] }
  | field COMMA field_list { $1 :: $3 }
  ;

const_list:
  | constant { [$1] }
  | constant COMMA const_list { $1 :: $3 }
  ;

delete:
  | DELETE FROM ID WHERE predicate {
      Query.Delete (Delete.make $3 $5)
    }
  ;

modify:
  | UPDATE ID SET field EQUALS expression WHERE predicate {
      Query.Update (Update.make $2 $4 $6 $8)
    }
  ;

create_table:
  | CREATE TABLE ID LPAREN field_defs RPAREN {
      let schema = Record_page.Schema.make () in
      List.iter (fun (field_name, field_type) ->
        match field_type with
        | LocalInteger -> Record_page.Schema.add_int_field schema field_name
        | LocalVarchar length -> Record_page.Schema.add_string_field schema field_name length
      ) $5;
      Query.CreateTable (Create_table.make $3 schema)
    }
  ;

field_defs:
  | field_def { [$1] }
  | field_def COMMA field_defs { $1 :: $3 }
  ;

field_def:
  | ID type_def { ($1, $2) }
  ;

type_def:
  | INT_TYPE { LocalInteger }
  | VARCHAR LPAREN INTEGER RPAREN { LocalVarchar $3 }
  ;

create_index:
  | CREATE INDEX ID ON ID LPAREN field RPAREN {
      Query.CreateIndex (Create_index.make $3 $5 $7)
    }
  ;

