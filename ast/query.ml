module Select = Select
module Create_table = Create_table

type t = 
  | Select of Select.t   
  | CreateTable of Create_table.t
  | Insert of Insert.t
  | Delete of Delete.t
  | Update of Update.t
  | CreateIndex of Create_index.t 

let rec to_string query =
  match query with
  | Select data -> Select.to_string data
  | CreateTable data -> Create_table.to_string data
  | Insert data -> Insert.to_string data
  | Delete data -> Delete.to_string data
  | Update data -> Update.to_string data
  | CreateIndex data -> Create_index.to_string data


