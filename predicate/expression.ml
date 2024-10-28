module Schema = Record_page__Schema

type t = Const of Constant.t | FieldName of string

exception NotConstExpr
exception NotFieldNameExpr

let make_const c = Const c
let make_field_name f = FieldName f
let is_field_name t = match t with Const _ -> false | _ -> true
let get_constant e = match e with Const c -> c | _ -> raise NotConstExpr

let get_field_name e =
  match e with Const _ -> raise NotFieldNameExpr | FieldName f -> f

let eval e s =
  match e with Const _ -> raise NotConstExpr | FieldName f -> Scan.get_val s f

let applies_to_schema e s =
  match e with Const _ -> true | FieldName f -> Schema.has_field s f

let to_string e =
  match e with Const c -> Constant.to_string c | FieldName f -> f
