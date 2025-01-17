module Predicate = Scans__Predicate
module Expression = Scans__Expression

type t = {
  tblname : string;
  fldname : string;
  newval : Expression.t;
  pred : Predicate.t;
}

let make tblname fldname newval pred = { tblname; fldname; newval; pred }
let table_name data = data.tblname
let field_name data = data.fldname
let new_value data = data.newval
let predicate data = data.pred

let to_string { tblname; fldname; newval; pred } =
  "UPDATE " ^ tblname ^ " SET " ^ fldname ^ " = "
  ^ Expression.to_string newval
  ^ " WHERE " ^ Predicate.to_string pred
