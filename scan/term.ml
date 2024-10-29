type t = { lhs : Expression.t; rhs : Expression.t }

let make lhs rhs = { lhs; rhs }

let is_satisfied { lhs; rhs } scan =
  let lval = Expression.eval lhs scan in
  let rval = Expression.eval rhs scan in
  Constant.equals lval rval

let applies_to { lhs; rhs } schema =
  Expression.applies_to_schema lhs schema
  && Expression.applies_to_schema rhs schema

let equates_with_constant { lhs; rhs } field_name =
  match (lhs, rhs) with
  | FieldName l_fieldname, Const r_const ->
      if l_fieldname = field_name then Some r_const else None
  | Const l_const, FieldName r_fieldname ->
      if r_fieldname = field_name then Some l_const else None
  | _ -> None

let equates_with_field { lhs; rhs } field_name =
  match (lhs, rhs) with
  | FieldName l_fieldname, FieldName r_fieldname ->
      if l_fieldname = field_name then Some r_fieldname
      else if r_fieldname = field_name then Some l_fieldname
      else None
  | _ -> None

let to_string { lhs; rhs } =
  Printf.sprintf "%s = %s" (Expression.to_string lhs) (Expression.to_string rhs)
