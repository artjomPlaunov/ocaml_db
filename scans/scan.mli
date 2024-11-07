class virtual t : object
  method virtual get_rid : Record_id.t
  method virtual move_to_rid : rid:Record_id.t -> unit
  method virtual delete : unit
  method virtual insert : unit
  method virtual set_string : field_name:string -> value:string -> unit
  method virtual set_int32 : field_name:string -> value:Int32.t -> unit
  method virtual set_val : field_name:string -> value:Constant.t -> unit
  method virtual before_first : unit
  method virtual next : bool
  method virtual get_int32 : field_name:string -> int32
  method virtual get_string : field_name:string -> string
  method virtual get_val : field_name:string -> Constant.t
  method virtual has_field : field_name:string -> bool
  method virtual close : unit
end

