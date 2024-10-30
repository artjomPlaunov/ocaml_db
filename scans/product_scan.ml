module type Scan_type = module type of Scan__Scan_type

module type Product_scan_arg = sig
  val scan : Scan.Scan_type.t
end

module rec Product_scanF : functor
  (S1 : module type of Scan.Scan_type)
  (S2 : module type of Scan.Scan_type)
  (Scan1 : Product_scan_arg)
  (Scan2 : Product_scan_arg)
  -> module type of Scan.Scan_type =
functor
  (S1 : module type of Scan.Scan_type)
  (S2 : module type of Scan.Scan_type)
  (Scan1 : Product_scan_arg)
  (Scan2 : Product_scan_arg)
  ->
  struct
    include S1

    (* Replace the make function, around lines 63-64 *)
    let make ~tx:_ ~tbl_name:_ ~layout:_ =
      Scan1.scan (* Return the first scan as our dummy value *)

    let before_first ~scan:_ =
      S1.before_first ~scan:Scan1.scan;
      let _ = S1.next ~scan:Scan1.scan in
      S2.before_first ~scan:Scan2.scan

    let next ~scan:_ =
      if S2.next ~scan:Scan2.scan then true
      else (
        S2.before_first ~scan:Scan2.scan;
        S2.next ~scan:Scan2.scan && S1.next ~scan:Scan1.scan)

    let get_int32 ~scan:_ ~field_name =
      if S1.has_field ~scan:Scan1.scan ~field_name then
        S1.get_int32 ~scan:Scan1.scan ~field_name
      else S2.get_int32 ~scan:Scan2.scan ~field_name

    let get_string ~scan:_ ~field_name =
      if S1.has_field ~scan:Scan1.scan ~field_name then
        S1.get_string ~scan:Scan1.scan ~field_name
      else S2.get_string ~scan:Scan2.scan ~field_name

    let get_val ~scan:_ ~field_name =
      if S1.has_field ~scan:Scan1.scan ~field_name then
        S1.get_val ~scan:Scan1.scan ~field_name
      else S2.get_val ~scan:Scan2.scan ~field_name

    let has_field ~scan:_ ~field_name =
      S1.has_field ~scan:Scan1.scan ~field_name
      || S2.has_field ~scan:Scan2.scan ~field_name

    let close ~scan:_ =
      S1.close ~scan:Scan1.scan;
      S2.close ~scan:Scan2.scan

    (* Unsupported operations that raise exceptions *)
    let make ~tx:_ ~tbl_name:_ ~layout:_ = Scan1.scan

    let move_to_rid ~scan:_ ~rid:_ =
      raise (Invalid_argument "move_to_rid not supported in product scan")

    let delete ~scan:_ =
      raise (Invalid_argument "delete not supported in product scan")

    let insert ~scan:_ =
      raise (Invalid_argument "insert not supported in product scan")

    let set_string ~scan:_ ~field_name:_ ~value:_ =
      raise (Invalid_argument "set_string not supported in product scan")

    let set_int32 ~scan:_ ~field_name:_ ~value:_ =
      raise (Invalid_argument "set_int32 not supported in product scan")

    let set_val ~scan:_ ~field_name:_ ~value:_ =
      raise (Invalid_argument "set_val not supported in product scan")

    let get_rid ~scan:_ =
      raise (Invalid_argument "get_rid not supported in product scan")
  end

let create_product_scan_arg scan_val =
  let module ScanArg : Product_scan_arg = struct
    let scan = scan_val
  end in
  (module ScanArg : Product_scan_arg)

let instantiate_product_scan scan_arg1 scan_arg2 =
  let module Scan1 = (val scan_arg1 : Product_scan_arg) in
  let module Scan2 = (val scan_arg2 : Product_scan_arg) in
  (module Product_scanF (Scan.Scan_type) (Scan.Scan_type) (Scan1) (Scan2)
  : Scan_type)
