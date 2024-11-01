type t = {
  tblname: string;
  pred: Predicate.t;
}

let make tblname pred = {
  tblname;
  pred;
}

let table_name data = data.tblname

let predicate data = data.pred

let to_string data =
  Printf.sprintf "DELETE FROM %s WHERE %s" 
    (table_name data) 
    (Predicate.to_string (predicate data))

