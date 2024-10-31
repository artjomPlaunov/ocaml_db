%{
  open Query_data
  open Predicate
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

%start <Query_data.query_data> prog

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
  | field { Predicate__Expression.make_field_name $1 }
  | constant { Predicate__Expression.make_const $1 }
  ;

term:
  | expression EQUALS expression { Predicate__Term.make $1 $3 }
  ;

predicate:
  | term { Predicate.make $1 }
  (* | term AND predicate { Predicate.and_ $1 $3 } *)
  ;

query:
  | SELECT select_list FROM table_list WHERE predicate { 
      Query_data.Select { fields = $2; tables = $4; predicate = Some $6 }
    }
  | SELECT select_list FROM table_list { 
      Query_data.Select { fields = $2; tables = $4; predicate = None }
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
  | create_view { $1 }
  | create_index { $1 }
  ;

insert:
  | INSERT INTO ID LPAREN field_list RPAREN VALUES LPAREN const_list RPAREN {
      Insert (Insert_data.make_insert_data $3 $5 $9)
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
      Delete (Delete_data.make_delete_data $3 $5)
    }
  ;

modify:
  | UPDATE ID SET field EQUALS expression WHERE predicate {
      Modify (Modify_data.make_modify_data $2 $4 $6 $8)
    }
  ;

create_table:
  | CREATE TABLE ID LPAREN field_defs RPAREN {
      let schema = Record_page__Schema.make () in
      List.iter (fun (field_name, field_type) ->
        match field_type with
        | LocalInteger -> Record_page__Schema.add_int_field schema field_name
        | LocalVarchar length -> Record_page__Schema.add_string_field schema field_name length
      ) $5;
      CreateTable (Create_table_data.make_create_table_data $3 schema)
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

create_view:
  | CREATE VIEW ID AS query {
      CreateView (make_create_view_data $3 $5)
    }
  ;

create_index:
  | CREATE INDEX ID ON ID LPAREN field RPAREN {
      CreateIndex (Create_index_data.make_create_index_data $3 $5 $7)
    }
  ;

