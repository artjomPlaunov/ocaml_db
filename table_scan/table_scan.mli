type t

val make : tx:Transaction.t -> tbl_name:string
  -> layout:Record_page__Layout.t -> t
val get_rid : tbl_scan:t -> Record_id.t
