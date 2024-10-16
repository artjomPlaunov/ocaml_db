type t

val make : tx:Transaction.t -> tbl_name:string
  -> layout:Record_page__Layout.t -> t
val get_rid : tbl_scan:t -> Record_id.t
val move_to_rid : tbl_scan:t -> rid:Record_id.t -> unit
val delete : tbl_scan:t -> unit
val insert : tbl_scan:t -> unit
val set_string : tbl_scan:t -> field_name:string -> value:string -> unit
val set_int32 : tbl_scan:t -> field_name:string -> value:Int32.t -> unit
val set_val : tbl_scan:t -> field_name:string -> value:Constant.t -> unit
val get_int32 : tbl_scan:t -> field_name:string -> Int32.t
val get_string : tbl_scan:t -> field_name:string -> string
val get_val : tbl_scan:t -> field_name:string -> Constant.t
val next : tbl_scan:t -> bool                                                   
val before_first : tbl_scan:t -> unit
val close : tbl_scan:t -> unit
