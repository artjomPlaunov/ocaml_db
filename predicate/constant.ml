type t = Integer of Int32.t | String of string

exception NotIntegerConstant
exception NotStringConstant
exception MismatchTypeCompare

let make_integer i32_val = Integer(i32_val)
let make_string string_val = String(string_val)

let get_integer constant = 
  match constant with 
  | Integer i32_val -> i32_val
  | String _ -> raise NotIntegerConstant

let get_string constant = 
  match constant with 
  | Integer _ -> raise NotStringConstant
  | String s_val -> s_val

let equals t1 t2 = 
  match t1, t2 with 
  | Integer _, String _ -> raise MismatchTypeCompare
  | String _, Integer _ -> raise MismatchTypeCompare
  | Integer i1, Integer i2 -> Int32.equal i1 i2
  | String s1, String s2 -> s1 = s2

let to_string t = 
  match t with 
  | Integer i32_val -> Int32.to_string i32_val
  | String s_val -> s_val
