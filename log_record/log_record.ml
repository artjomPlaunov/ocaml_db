open File

type t =
  | Checkpoint
  | Start of {
      mutable tx_num : int;
      mutable offset : int;
      mutable block : Block_id.t;
    }
  | Commit of {
      mutable tx_num : int;
      mutable offset : int;
      mutable block : Block_id.t;
    }
  | UpdateInt of {
      mutable tx_num : int;
      mutable offset : int;
      mutable value : Int32.t;
      mutable block : Block_id.t;
    }
  | UpdateString of {
      mutable tx_num : int;
      mutable offset : int;
      mutable value : string;
      mutable block : Block_id.t;
    }
  | Rollback of {
      mutable tx_num : int;
      mutable offset : int;
      mutable block : Block_id.t;
    }

let make_update_int_record page =
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
  UpdateInt { tx_num; offset; value; block }

let make_update_string_record page =
  let tx_pos = 4 in
  let filename_pos = tx_pos + 4 in
  let filename = Page.get_string page filename_pos in
  let block_pos = filename_pos + Page.max_len (String.length filename) in
  let block_num = Page.get_int32 page block_pos |> Int32.to_int in
  let offset_pos = block_pos + 4 in
  let value_pos = offset_pos + 4 in
  let value = Page.get_string page value_pos in
  let tx_num = Page.get_int32 page tx_pos |> Int32.to_int in
  let offset = Page.get_int32 page offset_pos |> Int32.to_int in
  let block = Block_id.make ~filename ~block_num in
  UpdateString { tx_num; offset; value; block }

let make_start_record page =
  let tx_pos = 4 in
  let filename_pos = tx_pos + 4 in
  let filename = Page.get_string page filename_pos in
  let block_pos = filename_pos + Page.max_len (String.length filename) in
  let block_num = Page.get_int32 page block_pos |> Int32.to_int in
  let offset_pos = block_pos + 4 in
  let tx_num = Page.get_int32 page tx_pos |> Int32.to_int in
  let offset = Page.get_int32 page offset_pos |> Int32.to_int in
  let block = Block_id.make ~filename ~block_num in
  Start { tx_num; offset; block }

let make_commit_record page =
  let tx_pos = 4 in
  let filename_pos = tx_pos + 4 in
  let filename = Page.get_string page filename_pos in
  let block_pos = filename_pos + Page.max_len (String.length filename) in
  let block_num = Page.get_int32 page block_pos |> Int32.to_int in
  let offset_pos = block_pos + 4 in
  let tx_num = Page.get_int32 page tx_pos |> Int32.to_int in
  let offset = Page.get_int32 page offset_pos |> Int32.to_int in
  let block = Block_id.make ~filename ~block_num in
  Commit { tx_num; offset; block }

let make_rollback_record page =
  let tx_pos = 4 in
  let filename_pos = tx_pos + 4 in
  let filename = Page.get_string page filename_pos in
  let block_pos = filename_pos + Page.max_len (String.length filename) in
  let block_num = Page.get_int32 page block_pos |> Int32.to_int in
  let offset_pos = block_pos + 4 in
  let tx_num = Page.get_int32 page tx_pos |> Int32.to_int in
  let offset = Page.get_int32 page offset_pos |> Int32.to_int in
  let block = Block_id.make ~filename ~block_num in
  Rollback { tx_num; offset; block }

let make ~byte =
  let page = Page.from_bytes byte in
  match Page.get_int32 page 0 |> Int32.to_int with
  | 0 -> failwith "checkpoint record"
  | 1 -> make_start_record page
  | 2 -> make_commit_record page
  | 3 -> make_update_int_record page
  | 4 -> make_update_string_record page
  | 5 -> make_rollback_record page
  | _ -> failwith "we're dead"
