open File

type t = {
  file_manager : File_manager.t;
  mutable block : Block_id.t;
  page : Page.t;
  mutable cur_pos : int;
  mutable boundary : int;
}

let make file_manager block =
  let page = Page.make ~block_size:(File_manager.get_blocksize file_manager) in
  File_manager.read file_manager block page;
  let boundary = Int32.to_int (Page.get_int32 page 0) in
  let cur_pos = boundary in
  { file_manager; block; page; cur_pos; boundary }

let has_next log_itr =
  log_itr.cur_pos < File_manager.get_blocksize log_itr.file_manager
  || Block_id.block_num log_itr.block > 0

let move_to_block log_itr block =
  File_manager.read log_itr.file_manager block log_itr.page;
  log_itr.boundary <- Int32.to_int (Page.get_int32 log_itr.page 0);
  log_itr.cur_pos <- log_itr.boundary

let next log_itr =
  let blocksize = File_manager.get_blocksize log_itr.file_manager in
  if log_itr.cur_pos = blocksize then (
    let filename = Block_id.file_name log_itr.block in
    let block_num = Block_id.block_num log_itr.block in
    log_itr.block <- Block_id.make ~filename ~block_num:(block_num - 1);
    move_to_block log_itr log_itr.block);
  let record = Page.get_bytes log_itr.page log_itr.cur_pos in
  log_itr.cur_pos <- log_itr.cur_pos + 4 + Bytes.length record;
  record
