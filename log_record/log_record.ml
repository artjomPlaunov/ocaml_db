open File

type t =
  | Checkpoint
  | Start of {
      mutable tx_num : int;
    }
  | Commit of {
      mutable tx_num : int;
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
    }

let make_checkpoint_record = 
  Checkpoint

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
  let tx_num = Page.get_int32 page tx_pos |> Int32.to_int in
  Start { tx_num; }

let make_commit_record page =
  let tx_pos = 4 in
  let tx_num = Page.get_int32 page tx_pos |> Int32.to_int in
  Commit { tx_num; }

let make_rollback_record page =
  let tx_pos = 4 in
  let tx_num = Page.get_int32 page tx_pos |> Int32.to_int in
  Rollback { tx_num; }

let write_checkpoint_log_record log_mgr = 
  let page = Page.make ~block_size:4 in 
  Page.set_int32 page 0 (Int32.of_int 0);
  Log_manager.append log_mgr (Page.contents page)

let write_start_log_record log_mgr tx_num = 
  let page = Page.make ~block_size:8 in 
  Page.set_int32 page 0 (Int32.of_int 1);
  Page.set_int32 page 4 (Int32.of_int tx_num);
  Log_manager.append log_mgr (Page.contents page)

let write_commit_log_record log_mgr tx_num = 
  let page = Page.make ~block_size:8 in 
  Page.set_int32 page 0 (Int32.of_int 2);
  Page.set_int32 page 4 (Int32.of_int tx_num);
  Log_manager.append log_mgr (Page.contents page)

let write_rollback_log_record log_mgr tx_num = 
  let page = Page.make ~block_size:8 in 
  Page.set_int32 page 0 (Int32.of_int 5);
  Page.set_int32 page 4 (Int32.of_int tx_num);
  Log_manager.append log_mgr (Page.contents page)

let write_update_int_log_record log_mgr tx_num blk offset value =
  let tx_pos = 4 in
  let fname_pos = tx_pos + 4 in
  let blk_num_pos =
    fname_pos + Page.max_len (String.length (Block_id.file_name blk))
  in
  let offset_pos = blk_num_pos + 4 in
  let value_pos = offset_pos + 4 in
  let rec_len = value_pos + 4 in
  let page = Page.make ~block_size:rec_len in
  Page.set_int32 page 0 (Int32.of_int 3);
  Page.set_int32 page tx_pos (Int32.of_int tx_num);
  Page.set_string page fname_pos (Block_id.file_name blk);
  Page.set_int32 page blk_num_pos (Int32.of_int (Block_id.block_num blk));
  Page.set_int32 page offset_pos (Int32.of_int offset);
  Page.set_int32 page value_pos value;
  Log_manager.append log_mgr (Page.contents page)

let write_update_string_log_record log_mgr tx_num blk offset value =
  let tx_pos = 4 in
  let fname_pos = tx_pos + 4 in
  let blk_num_pos =
    fname_pos + Page.max_len (String.length (Block_id.file_name blk))
  in
  let offset_pos = blk_num_pos + 4 in
  let value_pos = offset_pos + 4 in
  let rec_len = value_pos + Page.max_len (String.length value) in
  let page = Page.make ~block_size:rec_len in
  Page.set_int32 page 0 (Int32.of_int 4);
  Page.set_int32 page tx_pos (Int32.of_int tx_num);
  Page.set_string page fname_pos (Block_id.file_name blk);
  Page.set_int32 page blk_num_pos (Int32.of_int (Block_id.block_num blk));
  Page.set_int32 page offset_pos (Int32.of_int offset);
  Page.set_string page value_pos value;
  Log_manager.append log_mgr (Page.contents page)


let make ~byte =
  let page = Page.from_bytes byte in
  match Page.get_int32 page 0 |> Int32.to_int with
  | 0 -> make_checkpoint_record
  | 1 -> make_start_record page
  | 2 -> make_commit_record page
  | 3 -> make_update_int_record page
  | 4 -> make_update_string_record page
  | 5 -> make_rollback_record page
  | _ -> failwith "we're dead"

let to_string log_record =  
  match log_record with
  | Checkpoint -> Printf.sprintf "<CHECKPOINT>"
  | UpdateInt r ->
      Printf.sprintf "<UPDATE INT %d %s %d %d>" r.tx_num
        (File.Block_id.to_string r.block)
        r.offset
        (Int32.to_int r.value)
  | UpdateString r -> 
    Printf.sprintf "<UPDATE STRING %d %s %d %s>" r.tx_num
      (File.Block_id.to_string r.block)
      r.offset
      r.value
  | Commit r -> Printf.sprintf "<COMMIT %d>" r.tx_num
  | Rollback r -> Printf.sprintf "<ROLLBACK %d>" r.tx_num
  | Start r -> Printf.sprintf "<START %d>" r.tx_num
