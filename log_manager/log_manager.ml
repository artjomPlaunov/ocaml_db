open File

type t = {
  file_manager : File_manager.t;
  log_file : string;
  log_page : Page.t;
  mutable cur_block : Block_id.t;
  mutable latest_lsn : int;
  mutable last_saved_lsn : int;
  append_lock : Mutex.t;
}

let make ~file_manager ~log_file =
  let block_size = File_manager.get_blocksize file_manager in
  let log_page = Page.make ~block_size in
  let log_size = File_manager.size file_manager log_file in
  let cur_block =
    if log_size = 0 then (
      let block = File_manager.append file_manager log_file in
      Page.set_int32 log_page 0 (Int32.of_int block_size);
      File_manager.write file_manager block log_page;
      block)
    else
      let block = Block_id.make ~filename:log_file ~block_num:(log_size - 1) in
      File_manager.read file_manager block log_page;
      block
  in
  let latest_lsn = 0 in
  let last_saved_lsn = 0 in
  {
    file_manager;
    log_file;
    log_page;
    cur_block;
    latest_lsn;
    last_saved_lsn;
    append_lock = Mutex.create ();
  }

let append_new_block log_mgr =
  let log_page = log_mgr.log_page in
  let blocksize = File_manager.get_blocksize log_mgr.file_manager in
  let block = File_manager.append log_mgr.file_manager log_mgr.log_file in
  Page.set_int32 log_page 0 (Int32.of_int blocksize);
  File_manager.write log_mgr.file_manager block log_page;
  block

let flush_aux log_mgr =
  File_manager.write log_mgr.file_manager log_mgr.cur_block log_mgr.log_page;
  log_mgr.last_saved_lsn <- log_mgr.latest_lsn

let flush log_mgr lsn = if lsn >= log_mgr.last_saved_lsn then flush_aux log_mgr

let append log_mgr log_rec =
  Mutex.lock log_mgr.append_lock;
  let lsn =
    try
      let boundary = ref 0 in
      boundary := Int32.to_int (Page.get_int32 log_mgr.log_page 0);
      let rec_size = Bytes.length log_rec in
      let bytes_needed = rec_size + 4 in
      if !boundary - bytes_needed < 4 then (
        flush_aux log_mgr;
        log_mgr.cur_block <- append_new_block log_mgr;
        boundary := Int32.to_int (Page.get_int32 log_mgr.log_page 0));
      let rec_pos = !boundary - bytes_needed in
      Page.set_bytes log_mgr.log_page rec_pos log_rec;
      Page.set_int32 log_mgr.log_page 0 (Int32.of_int rec_pos);
      log_mgr.latest_lsn <- log_mgr.latest_lsn + 1;
      log_mgr.latest_lsn
    with e ->
      Mutex.unlock log_mgr.append_lock;
      raise e
  in
  Mutex.unlock log_mgr.append_lock;
  lsn

let get_iterator log_mgr =
  flush_aux log_mgr;
  Log_iterator.make ~file_manager:log_mgr.file_manager ~block:log_mgr.cur_block
