include module type of Scan

val get_rid : tbl_scan:t -> Record_id.t
val move_to_rid : tbl_scan:t -> rid:Record_id.t -> unit
val delete : tbl_scan:t -> unit
val insert : tbl_scan:t -> unit
val set_string : tbl_scan:t -> field_name:string -> value:string -> unit
val set_int32 : tbl_scan:t -> field_name:string -> value:Int32.t -> unit
val set_val : tbl_scan:t -> field_name:string -> value:Constant.t -> unit

