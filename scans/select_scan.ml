module type Scan_type = module type of Scan__Scan_type

module type Predicate_arg =
sig
  val predicate: Scan__Predicate.t
end

let create_predicate_arg predicate_val =
  (* Define MyPredicate inside this function *)
  let module PredArg : Predicate_arg = struct
    let predicate = predicate_val
  end in
  (module PredArg: Predicate_arg)



module rec Select_scanF : functor (S : module type of Scan.Scan_type ) (P: Predicate_arg) -> module type of Scan.Scan_type =
  functor (S : module type of Scan.Scan_type) (P: Predicate_arg) ->
  struct
    include S

    let next ~scan:t =
      let rec loop () =
        if S.next ~scan:t then (
          (if Scan__Predicate.is_satisfied P.predicate t then 
            true
          else
            loop ()))
        else
          false
    
      in
      loop ()
  end

let instantiate_select_scan predicate_arg =
    let module P = (val predicate_arg : Predicate_arg) in
    (module Select_scanF(Scan.Scan_type)(P) : Scan_type)