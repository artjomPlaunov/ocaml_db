module Schema = Record_page.Schema

type t = Const of Constant.t | FieldName of string

exception NotConstExpr
exception NotFieldNameExpr

let make_const c = Const c
let make_field_name f = FieldName f
let is_field_name term = match term with Const _ -> false | _ -> true
let get_constant expr = match expr with Const c -> c | _ -> raise NotConstExpr

let get_field_name expr =
  match expr with Const _ -> raise NotFieldNameExpr | FieldName f -> f

let eval expr scan =
  match expr with
  | Const c -> c
  | FieldName field_name -> scan#get_val ~field_name:field_name

let applies_to_schema expr schema =
  match expr with
  | Const _ -> true
  | FieldName field_name -> Schema.has_field schema field_name

let to_string expr =
  match expr with Const c -> Constant.to_string c | FieldName f -> f
