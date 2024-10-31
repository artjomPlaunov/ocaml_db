type query_data = 
  | Select of Select_data.select_data   
  | CreateTable of Create_table_data.create_table_data
  | Insert of Insert_data.insert_data
  | Delete of Delete_data.delete_data
  | Modify of Modify_data.modify_data
  | CreateView of create_view_data
  | CreateIndex of Create_index_data.create_index_data 

and create_view_data = {
  viewname: string;
  qrydata: query_data;
}

let make_create_view_data viewname qrydata = {
  viewname;
  qrydata;
}

let view_name data = data.viewname

let rec to_string query =
  match query with
  | Select data -> Select_data.to_string data
  | CreateTable data -> Create_table_data.to_string data
  | Insert data -> Insert_data.to_string data
  | Delete data -> Delete_data.to_string data
  | Modify data -> Modify_data.to_string data
  | CreateView data -> "CREATE VIEW " ^ data.viewname ^ " AS " ^ to_string data.qrydata
  | CreateIndex data -> Create_index_data.to_string data 

let view_def data =
  let query_data = data.qrydata in
  to_string query_data

