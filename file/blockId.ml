type t = string * int

let file_name (f,_) = f
let block_num (_,x) = x
let make s x = (s,x)
