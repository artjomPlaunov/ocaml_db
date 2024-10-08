type t = string * int

let make ~filename ~block_num = (filename, block_num)
let file_name (filename, _) = filename
let block_num (_, block_num) = block_num
let to_string (s, n) = Printf.sprintf "%s, %d" s n
let eq (s1, n1) (s2, n2) = s1 = s2 && n1 = n2
