type t = { block_num : int; slot : int }

let make ~block_num ~slot = { block_num; slot }
let get_block_num ~rid = rid.block_num
let get_slot ~rid = rid.slot
let eq ~rid1 ~rid2 = rid1.block_num = rid2.block_num && rid1.slot = rid2.slot
let to_string ~rid = Printf.sprintf "[%d, %d]" rid.block_num rid.slot
