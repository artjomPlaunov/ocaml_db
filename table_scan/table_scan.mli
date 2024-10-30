class t : 
  Transaction.t -> 
  string ->  (* table name *)
  Record_page__Layout.t -> 
  object
    inherit Scan.t
    method get_rid : Record_id.t
    method move_to_rid : rid:Record_id.t -> unit
    method delete : unit
    method insert : unit
    method set_string : field_name:string -> value:string -> unit
    method set_int32 : field_name:string -> value:Int32.t -> unit
    method set_val : field_name:string -> value:Constant.t -> unit
    method get_int32 : field_name:string -> Int32.t
    method get_string : field_name:string -> string
    method get_val : field_name:string -> Constant.t
    method has_field : field_name:string -> bool
    method before_first : unit
    method next : bool
    method close : unit
  end

val make : tx:Transaction.t -> tbl_name:string -> layout:Record_page__Layout.t -> t
