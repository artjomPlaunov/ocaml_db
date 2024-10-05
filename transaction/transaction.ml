open File

type t = {
  next_tx_num : int;
  eof : int;
  buffer_manager : Buffer_manager.t;
  file_manager : File_manager.t;
  log_manager : Log_manager.t;
  tx_num : int;
}

let make ~file_manager ~log_manager ~buffer_manager =
  {
    next_tx_num = 0;
    eof = -1;
    buffer_manager;
    file_manager;
    log_manager;
    tx_num = -1;
  }

let commit transaction = failwith "todo"
let rollback transaction = failwith "todo"
let recover transaction = failwith "todo"
