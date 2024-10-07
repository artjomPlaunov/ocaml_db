open File

type t = {
  next_tx_num : int;
  eof : int;
  buffer_manager : Buffer_manager.t;
  file_manager : File_manager.t;
  log_manager : Log_manager.t;
  tx_num : int;
  buffers: Transaction__Buffer_list.t;
}

let make ~file_manager ~log_manager ~buffer_manager =
  {
    next_tx_num = 0;
    eof = -1;
    buffer_manager;
    file_manager;
    log_manager;
    tx_num = -1;
    buffers =  Buffer_list.make ~buffer_mgr:buffer_manager;
  }

let commit tx =
  (* recoveryMgr.commit code *)
  Buffer_manager.flush_all tx.buffer_manager tx.tx_num;
  let lsn = Log_record.write_commit_log_record tx.log_manager tx.tx_num in
  Log_manager.flush tx.log_manager lsn;
  Transaction__Buffer_list 
  Printf.printf "transaction %d committed" tx.tx_num
  (* concurMgr.release() *)

  
let rollback transaction = failwith "todo"
let recover transaction = failwith "todo"
