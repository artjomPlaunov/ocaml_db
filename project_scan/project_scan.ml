class t (scan : #Scan.t) (fieldlist : string list) = object(self)
  method before_first : unit = 
    scan#before_first

  method next : bool = 
    scan#next

  method get_int32 ~(field_name : string) : int32 =
    if self#has_field ~field_name then
      scan#get_int32 ~field_name
    else
      raise (Invalid_argument "field not found")

  method get_string ~(field_name : string) : string =
    if self#has_field ~field_name then
      scan#get_string ~field_name
    else
      raise (Invalid_argument "field not found")

  method get_val ~(field_name : string) : Constant.t =
    if self#has_field ~field_name then
      scan#get_val ~field_name
    else
      raise (Invalid_argument "field not found")

  method has_field ~(field_name : string) : bool =
    List.mem field_name fieldlist

  method close : unit =
    scan#close

  method set_int32 ~(field_name:string) ~(value:int32) : unit =
    raise (Invalid_argument "operation not supported")

  method set_string ~(field_name:string) ~(value:string) : unit =
    raise (Invalid_argument "operation not supported")

  method set_val ~(field_name:string) ~(value:Constant.t) : unit =
    raise (Invalid_argument "operation not supported")

  method delete : unit =
    raise (Invalid_argument "operation not supported")

  method insert : unit =
    raise (Invalid_argument "operation not supported")

  method get_rid : Record_id.t =
    raise (Invalid_argument "operation not supported")

  method move_to_rid ~(rid:Record_id.t) : unit =
    raise (Invalid_argument "operation not supported")
end