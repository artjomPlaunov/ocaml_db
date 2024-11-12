type t = Varchar of int | Integer

let size key = match key with 
| Varchar (_,n) -> n
| Integer _ -> 4

let cmp key1 key2 = 
  match key1, key2 with 
  | Varchar (s1,_), Varchar (s2,_) -> String.compare s1 s2 
  | Integer(n1), Integer(n2) -> compare n1 n2
  | _ -> failwith "invalid key compare"

  