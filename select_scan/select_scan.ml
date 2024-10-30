class t (scan : #Scan.t) (pred : Predicate.t) = object(self)
  method get_rid = scan#get_rid
  method move_to_rid ~rid = scan#move_to_rid ~rid
  method delete = scan#delete
  method insert = scan#insert
  method set_string ~field_name ~value = scan#set_string ~field_name ~value
  method set_int32 ~field_name ~value = scan#set_int32 ~field_name ~value
  method set_val ~field_name ~value = scan#set_val ~field_name ~value
  method before_first = scan#before_first
  method next =
    let rec try_next () =
      if scan#next then
        if Predicate.is_satisfied pred scan then true
        else try_next ()
      else false
    in
    try_next ()
  method get_int32 ~field_name = scan#get_int32 ~field_name
  method get_string ~field_name = scan#get_string ~field_name
  method get_val ~field_name = scan#get_val ~field_name
  method has_field ~field_name = scan#has_field ~field_name
  method close = scan#close
end 