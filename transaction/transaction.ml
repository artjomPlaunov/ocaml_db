type t = {
  next_tx_num : int;
  eof         : int;
  buffer_mgr  : Buffer_manager.t;
  file_mgr    : File.File_manager.t;
  log_mgr     : Log_manager.t;
  tx_num      : int;
}

let make file_mgr log_mgr buffer_mgr = 
  { next_tx_num = 0; eof = -1; buffer_mgr; file_mgr; log_mgr; tx_num = -1}