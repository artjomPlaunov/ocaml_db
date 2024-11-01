type t = {
  tblname: string;
  fldname: string;
  newval: Predicate__Expression.t;
  pred: Predicate.t;
}

let make tblname fldname newval pred = {
  tblname;
  fldname;
  newval;
  pred;
}

let table_name data = data.tblname

let field_name data = data.fldname

let new_value data = data.newval

let predicate data = data.pred

let to_string { tblname; fldname; newval; pred } =
  "UPDATE " ^ tblname ^ " SET " ^ fldname ^ " = " ^ Predicate__Expression.to_string newval ^
  " WHERE " ^ Predicate.to_string pred