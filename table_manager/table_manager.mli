module Schema = Record_page.Schema
module Layout = Record_page.Layout

type t = {
  max_name : int;
  mutable table_catalog_layout : Layout.t;
  mutable field_catalog_layout : Layout.t;
}

val create_table :
  table_mgr:t -> tbl_name:string -> schema:Schema.t -> tx:Transaction.t -> unit

val make : is_new:bool -> tx:Transaction.t -> t
val get_layout : table_mgr:t -> tbl_name:string -> tx:Transaction.t -> Layout.t
