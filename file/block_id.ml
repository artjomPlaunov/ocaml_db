type t = string * int

let file_name (f, _) = f
let block_num (_, n) = n
let make ~filename ~block_num = (filename, block_num)
