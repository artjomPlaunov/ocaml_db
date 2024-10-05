open File

type value_t = StringVal of string | IntVal of int

type log_record_kind =
  | Checkpoint
  | Start
  | Commit
  | Rollback
  | SetInt
  | SetString

type t = {
  kind : log_record_kind;
  mutable tx_num : int;
  mutable offset : int;
  mutable value : value_t;
  mutable block : Block_id.t;
}

let set_record log_record page =
  let tx_pos = 4 in
  let filename_pos = tx_pos + 4 in
  let filename = Page.get_string page filename_pos in
  let block_pos = filename_pos + Page.max_len (String.length filename) in
  let block_num = Page.get_int32 page block_pos |> Int32.to_int in
  let offset_pos = block_pos + 4 in
  let value_pos = offset_pos + 4 in
  let value = Page.get_string page value_pos in
  log_record.tx_num <- Page.get_int32 page tx_pos |> Int32.to_int;
  log_record.offset <- Page.get_int32 page offset_pos |> Int32.to_int;
  log_record.block <- Block_id.make ~filename ~block_num
