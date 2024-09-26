type t = bytes

let make block_size = Bytes.make block_size '0'

let get_int32 page offset = Bytes.get_int32_ne page offset

let set_int32 page offset n = Bytes.set_int32_ne page offset n
  
  
