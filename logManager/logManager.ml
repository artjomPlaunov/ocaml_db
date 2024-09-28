open File

(* exception NotImplemented *)

type t = {
  _fm             : FileManager.t;
  _log_file       : string;
  _log_page       : Page.t;
  _cur_block      : BlockId.t; 
  _latest_lsn     : int;
  _last_saved_lsn : int;
}

let test s = s

let make _fm _log_file = 
  let blocksize = FileManager.get_blocksize _fm in
  let _log_page = Page.make blocksize in
  let _log_size = FileManager.size _fm _log_file in  
  let _cur_block =
    if _log_size = 0 
    then 
      let b = FileManager.append _fm _log_file in 
      let _ = Page.set_int32 _log_page 0 (Int32.of_int blocksize) in 
      let _ = FileManager.write _fm b _log_page in 
      b 
    else
      let b = BlockId.make _log_file (_log_size - 1) in 
      let _ = FileManager.read _fm b _log_page in 
      b
    in 
  {_fm; _log_file; _log_page; _cur_block; _latest_lsn = 0; _last_saved_lsn = 0}
