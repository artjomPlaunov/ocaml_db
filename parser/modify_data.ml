type modify_data = {
  tblname: string;
  fldname: string;
  newval: Predicate__Expression.t;
  pred: Predicate.t;
}

let make_modify_data tblname fldname newval pred = {
  tblname;
  fldname;
  newval;
  pred;
}

let table_name data = data.tblname

let target_field data = data.fldname

let new_value data = data.newval

let predicate data = data.pred

let to_string { tblname; fldname; newval; pred } =
  "UPDATE " ^ tblname ^ " SET " ^ fldname ^ " = " ^ Predicate__Expression.to_string newval ^
  " WHERE " ^ Predicate.to_string pred