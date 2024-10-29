module rec Product_scan : functor (S1 : module type of Scan.Scan_type ) (S2: module type of Scan.Scan_type) -> 
  module type of Scan.Scan_type =
  functor (S1 : module type of Scan.Scan_type) (S2 : module type of Scan.Scan_type) ->
  struct
  include S1
  end