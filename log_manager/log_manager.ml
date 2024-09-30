open File

type t = {
  file_manager : File_manager.t;
  log_file : string;
  log_page : Page.t;
  mutable cur_block : Block_id.t;
  mutable latest_lsn : int;
  mutable last_saved_lsn : int;
}

let test s = s

let make ~file_manager ~log_file =
  
  let blocksize = File_manager.get_blocksize file_manager in
  let log_page = Page.make blocksize in
  let log_size = File_manager.size file_manager log_file in

  let cur_block =
    if log_size = 0 then
      let block = File_manager.append file_manager log_file in
      let _ = Page.set_int32 log_page 0 (Int32.of_int blocksize) in
      let _ = File_manager.write file_manager block log_page in
      block
    else
      let block = Block_id.make ~filename:log_file ~block_num:(log_size - 1) in
      let _ = File_manager.read file_manager block log_page in
      block
  in
  let latest_lsn = 0 in
  let last_saved_lsn = 0 in
  { file_manager; log_file; log_page; cur_block; latest_lsn; last_saved_lsn }

let append_new_block log_mgr =
  let log_page = log_mgr.log_page in
  let blocksize = File_manager.get_blocksize log_mgr.file_manager in
  let block = File_manager.append log_mgr.file_manager log_mgr.log_file in
  let _ = Page.set_int32 log_page 0 (Int32.of_int blocksize) in
  let _ = File_manager.write log_mgr.file_manager block log_page in
  block

let flush_aux log_mgr =
  let _ =
    File_manager.write log_mgr.file_manager log_mgr.cur_block log_mgr.log_page
  in
  log_mgr.last_saved_lsn <- log_mgr.latest_lsn

let flush log_mgr lsn = if lsn >= log_mgr.latest_lsn then flush_aux log_mgr

(* append also returns the latest lsn *)
(* TODO think about if we want to rename or separate out the lsn return *)
let append log_mgr log_rec =
  let boundary = ref 0 in
  let _ = boundary := Int32.to_int (Page.get_int32 log_mgr.log_page 0) in
  let rec_size = Bytes.length log_rec in
  let bytes_needed = rec_size + 4 in
  let _ =
    if !boundary - bytes_needed < 4 then
      let _ = flush_aux log_mgr in
      let _ = log_mgr.cur_block <- append_new_block log_mgr in
      boundary := Int32.to_int (Page.get_int32 log_mgr.log_page 0)
    else ()
  in
  let rec_pos = !boundary - bytes_needed in
  let _ = Page.set_bytes log_mgr.log_page rec_pos log_rec in
  let _ = Page.set_int32 log_mgr.log_page 0 (Int32.of_int rec_pos) in
  log_mgr.latest_lsn <- log_mgr.latest_lsn + 1;
  log_mgr.latest_lsn
