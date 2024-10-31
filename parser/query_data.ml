type field = string

type table = string

type select_data = {
  fields: field list;
  tables: table list;
  predicate: Predicate.t option;
}

type create_table_data = {
  table_name: string;
  fields: (string * [ `Int | `String of int ]) list;
}

type query_data = 
  | Select of select_data
  | CreateTable of create_table_data 