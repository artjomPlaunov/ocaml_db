module Schema = Record_page.Schema
module Layout = Record_page.Layout
module Type = Record_page.Type
module Table_scan = Scans__Table_scan

type t = {
  max_name_len : int;
  index_layout : Layout.t;
}

let create_index_table ~tx ~index_mgr =
  let layout = index_mgr.index_layout in 
  let scan = Table_scan.make ~tx ~tbl_name:"index.dat" ~layout in 
  scan#insert;
  
  ()


let make ~tx ~is_new = 
  let index_schema = Schema.make () in 
  Schema.add_string_field index_schema "tablename" 16;
  Schema.add_string_field index_schema "constraint" 100;
  let layout = Layout.make index_schema in 
  (* create table here if necessary*)
  if is_new then (
    failwith "todo"
  );
  {max_name_len=100;index_layout = layout}
  
  
  
