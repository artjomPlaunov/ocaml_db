class t (scan1 : #Scan.t) (scan2 : #Scan.t) = object(self)
  (* Initializes the scan by moving the first scan to the next position *)
  initializer ignore scan1#next

  method before_first : unit =
    scan1#before_first;
    ignore (scan1#next);
    scan2#before_first

  method next : bool =
    if scan2#next then
      true
    else (
      scan2#before_first;
      scan2#next && scan1#next
    )

  method get_int32 ~(field_name : string) : int32 =
    if scan1#has_field ~field_name then
      scan1#get_int32 ~field_name
    else
      scan2#get_int32 ~field_name

  method get_string ~(field_name : string) : string =
    if scan1#has_field ~field_name then
      scan1#get_string ~field_name
    else
      scan2#get_string ~field_name

  method get_val ~(field_name : string) : Constant.t =
    if scan1#has_field ~field_name then
      scan1#get_val ~field_name
    else
      scan2#get_val ~field_name

  method has_field ~(field_name : string) : bool =
    scan1#has_field ~field_name || scan2#has_field ~field_name

  method close : unit =
    scan1#close;
    scan2#close

  (* Methods not implemented in ProductScan, raising errors at runtime *)
  method get_rid : Record_id.t =
    failwith "Error: attempting to get record ID from a product scan."

  method move_to_rid ~(rid : Record_id.t) : unit =
    failwith "Error: attempting to move to record ID in a product scan."

  method delete : unit =
    failwith "Error: delete operation not supported in a product scan."

  method insert : unit =
    failwith "Error: insert operation not supported in a product scan."

  method set_int32 ~(field_name : string) ~(value : int32) : unit =
    failwith "Error: set_int32 operation not supported in a product scan."

  method set_string ~(field_name : string) ~(value : string) : unit =
    failwith "Error: set_string operation not supported in a product scan."

  method set_val ~(field_name : string) ~(value : Constant.t) : unit =
    failwith "Error: set_val operation not supported in a product scan."
end
