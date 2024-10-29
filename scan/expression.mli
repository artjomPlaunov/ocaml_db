module Schema = Record_page__Schema

type t = Const of Constant.t | FieldName of string

exception NotConstExpr
exception NotFieldNameExpr

val make_const : Constant.t -> t
val make_field_name : string -> t
val is_field_name : t -> bool
val get_constant : t -> Constant.t
val get_field_name : t -> string
val eval : t -> Scan_type.t -> Constant.t
val applies_to_schema : t -> Schema.t -> bool
val to_string : t -> string
