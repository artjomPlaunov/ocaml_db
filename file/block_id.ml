type t = string * int

let file_name (filename, _) = filename
let block_num (_, block_num) = block_num
let make ~filename ~block_num = (file_name, block_num)
