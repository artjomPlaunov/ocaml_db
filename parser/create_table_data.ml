type t = {
  tblname: string;
  sch: Record_page.Schema.t;
}

let make tblname sch = {
  tblname;
  sch;
}

let table_name data = data.tblname

let new_schema data = data.sch

let to_string { tblname; sch } =
  let fields_str = Record_page.Schema.to_string sch in
  "CREATE TABLE " ^ tblname ^ " (" ^ fields_str ^ ")"