type insert_data = {
  tblname: string;
  flds: string list;
  vals: Constant.t list;
}

let make_insert_data tblname flds vals = {
  tblname;
  flds;
  vals;
}

let table_name data = data.tblname

let fields data = data.flds

let values data = data.vals

let to_string { tblname; flds; vals } =
  let fields_str = String.concat ", " flds in
  let values_str = String.concat ", " (List.map Constant.to_string vals) in
  "INSERT INTO " ^ tblname ^ " (" ^ fields_str ^ ") VALUES (" ^ values_str ^ ")"