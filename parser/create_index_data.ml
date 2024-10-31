type create_index_data = {
  idxname: string;
  tblname: string;
  fldname: string;
}

let make_create_index_data idxname tblname fldname = {
  idxname;
  tblname;
  fldname;
}

let index_name data = data.idxname

let table_name data = data.tblname

let field_name data = data.fldname

let to_string { idxname; tblname; fldname } =
  "CREATE INDEX " ^ idxname ^ " ON " ^ tblname ^ " (" ^ fldname ^ ")"