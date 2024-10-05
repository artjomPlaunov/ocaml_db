open File

type t = {
  mutable tx_num : int;
  mutable offset : int;
  mutable value : Int32.t;
  mutable block : Block_id.t;
}

let make ~page =
  let tx_pos = 4 in
  let filename_pos = tx_pos + 4 in
  let filename = Page.get_string page filename_pos in
  let block_pos = filename_pos + Page.max_len (String.length filename) in
  let block_num = Page.get_int32 page block_pos |> Int32.to_int in
  let offset_pos = block_pos + 4 in
  let value_pos = offset_pos + 4 in
  let value = Page.get_int32 page value_pos in
  let tx_num = Page.get_int32 page tx_pos |> Int32.to_int in
  let offset = Page.get_int32 page offset_pos |> Int32.to_int in
  let block = Block_id.make ~filename ~block_num in
  { tx_num; offset; value; block }
