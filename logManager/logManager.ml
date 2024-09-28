open File

type t = {
  _fm : FileManager.t;
  _log_file : string;
  _log_page : Page.t;
  _cur_block : BlockId.t ref;
  _latest_lsn : int ref;
  _last_saved_lsn : int ref;
}

let test s = s

let make _fm _log_file =
  let blocksize = FileManager.get_blocksize _fm in
  let _log_page = Page.make blocksize in
  let _log_size = FileManager.size _fm _log_file in
  let _cur_block =
    if _log_size = 0 then
      let b = FileManager.append _fm _log_file in
      let _ = Page.set_int32 _log_page 0 (Int32.of_int blocksize) in
      let _ = FileManager.write _fm b _log_page in
      b
    else
      let b = BlockId.make _log_file (_log_size - 1) in
      let _ = FileManager.read _fm b _log_page in
      b
  in
  let _cur_block = ref _cur_block in
  let _latest_lsn = ref 0 in
  let _last_saved_lsn = ref 0 in
  { _fm; _log_file; _log_page; _cur_block; _latest_lsn; _last_saved_lsn }

let append_new_block log_mgr =
  let log_page = log_mgr._log_page in
  let blocksize = FileManager.get_blocksize log_mgr._fm in
  let block = FileManager.append log_mgr._fm log_mgr._log_file in
  let _ = Page.set_int32 log_page 0 (Int32.of_int blocksize) in
  let _ = FileManager.write log_mgr._fm block log_page in
  block

let flush_aux log_mgr =
  let _ =
    FileManager.write log_mgr._fm !(log_mgr._cur_block) log_mgr._log_page
  in
  log_mgr._last_saved_lsn := !(log_mgr._latest_lsn)

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
  let _ = log_mgr._latest_lsn := !(log_mgr._latest_lsn) + 1 in
  !(log_mgr._latest_lsn)
