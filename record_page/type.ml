type t = Integer | Varchar

let to_int = function Integer -> 0 | Varchar -> 1

let of_int = function
  | 0 -> Integer
  | 1 -> Varchar
  | _ -> failwith "type doesn't exist for this integer"
