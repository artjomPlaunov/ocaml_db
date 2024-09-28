open File

exception NotImplemented

type t = {
  _fm             : FileManager.t;
  _log_file       : string;
  _log_page       : Page.t;
  _cur_block      : BlockId.t; 
  _latest_lsn     : int;
  _last_saved_lsn : int;
}

let test s = s

let make file_mgr logfile = 
  let _log_page = Page.make (FileManager.get_blocksize file_mgr) in
  let _log_size = FileManager.size file_mgr logfile in  
  raise NotImplemented 


