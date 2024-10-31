module Schema = Record_page__Schema
module Layout = Record_page__Layout

type t

val make :
  ?max_view_defn:int ->
  is_new:bool ->
  table_mgr:Table_manager.t ->
  tx:Transaction.t ->
  t

val create_view :
  t -> view_name:string -> view_defn:string -> tx:Transaction.t -> unit

val get_view_defn_opt :
  t -> view_name:string -> tx:Transaction.t -> string option
