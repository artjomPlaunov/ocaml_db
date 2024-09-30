type t = string * int

let make ~filename ~block_num = (filename, block_num)
let file_name (filename, _) = filename
let block_num (_, block_num) = block_num