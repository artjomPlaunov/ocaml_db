open File

type t = {
  _fm : File_manager.t;
  _log_file : string;
  _log_page : Page.t;
  _cur_block : Block_id.t ref;
  mutable latest_lsn : int;
  mutable last_saved_lsn : int;
}

let test s = s

let make _fm _log_file =
  let blocksize = File_manager.get_blocksize _fm in
  let _log_page = Page.make blocksize in
  let _log_size = File_manager.size _fm _log_file in
  let _cur_block =
    if _log_size = 0 then
      let b = File_manager.append _fm _log_file in
      let _ = Page.set_int32 _log_page 0 (Int32.of_int blocksize) in
      let _ = File_manager.write _fm b _log_page in
      b
    else
      let b = Block_id.make ~filename:_log_file ~block_num:(_log_size - 1) in
      let _ = File_manager.read _fm b _log_page in
      b
  in
  let _cur_block = ref _cur_block in
  let latest_lsn = 0 in
  let last_saved_lsn = 0 in
  { _fm; _log_file; _log_page; _cur_block; latest_lsn; last_saved_lsn }

let append_new_block log_mgr =
  let log_page = log_mgr._log_page in
  let blocksize = File_manager.get_blocksize log_mgr._fm in
  let block = File_manager.append log_mgr._fm log_mgr._log_file in
  let _ = Page.set_int32 log_page 0 (Int32.of_int blocksize) in
  let _ = File_manager.write log_mgr._fm block log_page in
  block

let flush_aux log_mgr =
  let _ =
    File_manager.write log_mgr._fm !(log_mgr._cur_block) log_mgr._log_page
  in
  log_mgr.last_saved_lsn <- log_mgr.latest_lsn

let flush log_mgr lsn = if lsn >= log_mgr.latest_lsn then flush_aux log_mgr

let append log_mgr log_rec =
  let boundary = ref 0 in
  let _ = boundary := Int32.to_int (Page.get_int32 log_mgr._log_page 0) in
  let rec_size = Bytes.length log_rec in
  let bytes_needed = rec_size + 4 in
  let _ =
    if !boundary - bytes_needed < 4 then
      let _ = flush_aux log_mgr in
      let _ = log_mgr._cur_block := append_new_block log_mgr in
      boundary := Int32.to_int (Page.get_int32 log_mgr._log_page 0)
    else ()
  in
  let recpos = !boundary - bytes_needed in
  let _ = Page.set_bytes log_mgr._log_page recpos log_rec in
  let _ = Page.set_int32 log_mgr._log_page 0 (Int32.of_int recpos) in
  log_mgr.latest_lsn <- log_mgr.latest_lsn + 1;
  log_mgr.latest_lsn
