module type Predicate_arg =
sig
  val predicate: Scan__Predicate.t
end

module rec Select_scan : functor (S : module type of Scan.Scan_type ) (P: Predicate_arg) -> module type of Scan.Scan_type =
  functor (S : module type of Scan.Scan_type) (P: Predicate_arg) ->
  struct
    include S
  end