type t = 
  | Select of Select_data.t   
  | CreateTable of Create_table_data.t
  | Insert of Insert_data.t
  | Delete of Delete_data.t
  | Modify of Modify_data.t
  | CreateView of view_data
  | CreateIndex of Create_index_data.t 

and view_data = {
  viewname: string;
  qrydata: t;
}

let make_view_data viewname qrydata = {
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

