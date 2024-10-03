type t = bytes

let make ~block_size = Bytes.make block_size '\000'
let from_bytes b = b
let get_int32 page offset = Bytes.get_int32_ne page offset
let set_int32 page offset n = Bytes.set_int32_ne page offset n
let contents page = page

let get_bytes page offset =
  let len = Int32.to_int (get_int32 page offset) in
  Bytes.sub page (offset + 4) len

let set_bytes page offset b =
  assert (offset + 4 + Bytes.length b <= Bytes.length page);
  let len = Bytes.length b in
  let _ = set_int32 page offset (Int32.of_int len) in
  Bytes.blit b 0 page (offset + 4) len

let get_string page offset = Bytes.to_string (get_bytes page offset)
let set_string page offset s = set_bytes page offset (Bytes.of_string s)

(* only ascii encoding for now *)
let max_len l = 4 + l
let ( = ) = Bytes.equal
let zero_out page = Bytes.fill page 0 (Bytes.length page) '\000'
