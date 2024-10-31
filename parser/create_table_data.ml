type create_table_data = {
  tblname: string;
  sch: Record_page__Schema.t;
}

let make_create_table_data tblname sch = {
  tblname;
  sch;
}

let table_name data = data.tblname

let new_schema data = data.sch

let to_string { tblname; sch } =
  let fields_str = Record_page__Schema.to_string sch in
  "CREATE TABLE " ^ tblname ^ " (" ^ fields_str ^ ")"