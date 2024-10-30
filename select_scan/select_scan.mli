class t :
  Scan.t ->
  Predicate.t ->
  object
    inherit Scan.t
    method before_first : unit
    method close : unit
    method delete : unit
    method get_int32 : field_name:string -> int32
    method get_rid : Record_id.t
    method get_string : field_name:string -> string
    method get_val : field_name:string -> Constant.t
    method has_field : field_name:string -> bool
    method insert : unit
    method move_to_rid : rid:Record_id.t -> unit
    method next : bool
    method set_int32 : field_name:string -> value:int32 -> unit
    method set_string : field_name:string -> value:string -> unit
    method set_val : field_name:string -> value:Constant.t -> unit
  end