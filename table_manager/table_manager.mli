module Schema = Record_page__Schema
module Layout = Record_page__Layout

type t

val create_table :
  table_mgr:t -> tbl_name:string -> schema:Schema.t -> tx:Transaction.t -> unit

val make : is_new:bool -> tx:Transaction.t -> t
val get_layout : table_mgr:t -> tbl_name:string -> tx:Transaction.t -> Layout.t
