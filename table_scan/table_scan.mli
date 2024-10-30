type t = Scan.Scan_type.t

val make :
  tx:Transaction.t -> tbl_name:string -> layout:Record_page__Layout.t -> t

val get_rid : scan:t -> Record_id.t
val move_to_rid : scan:t -> rid:Record_id.t -> unit
val delete : scan:t -> unit
val insert : scan:t -> unit
val has_field : scan:t -> field_name:string -> bool
val set_string : scan:t -> field_name:string -> value:string -> unit
val set_int32 : scan:t -> field_name:string -> value:Int32.t -> unit
val set_val : scan:t -> field_name:string -> value:Constant.t -> unit
val get_int32 : scan:t -> field_name:string -> Int32.t
val get_string : scan:t -> field_name:string -> string
val get_val : scan:t -> field_name:string -> Constant.t
val next : scan:t -> bool
val before_first : scan:t -> unit
val close : scan:t -> unit