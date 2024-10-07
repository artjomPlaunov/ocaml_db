open File

type t = {
  next_tx_num : int;
  eof : int;
  buffer_manager : Buffer_manager.t;
  file_manager : File_manager.t;
  log_manager : Log_manager.t;
  tx_num : int;
  buffers : Transaction__Buffer_list.t;
}

let make ~file_manager ~log_manager ~buffer_manager =
  {
    next_tx_num = 0;
    eof = -1;
    buffer_manager;
    file_manager;
    log_manager;
    tx_num = -1;
    buffers = Buffer_list.make ~buffer_mgr:buffer_manager;
  }

let set_string ~tx ~block ~offset ~value ~to_log =
  (* concurrency manager lock *)
  let buf = Transaction__Buffer_list.get_buffer ~buf_list:tx.buffers ~block in 
  let lsn = ref (-1) in
  ()

let set_int ~tx ~block ~offset ~value ~to_log =
  let buffer = Transaction__Buffer_list.get_buffer ~buf_list:tx.buffers ~block in
  let page = Buffer_manager__Db_buffer.contents buffer in
  let lsn = if to_log then (
      let old_val = Page.get_int32 page offset in
      let block = Buffer_manager__Db_buffer.block buffer in
      Log_record.write_update_int_log_record tx.log_manager tx.tx_num block offset old_val
    ) else -1 in
  Page.set_int32 page offset value;
  Buffer_manager__Db_buffer.set_modified buffer tx.tx_num lsn;

let commit tx =
  (* recoveryMgr.commit code *)
  Buffer_manager.flush_all tx.buffer_manager tx.tx_num;
  let lsn = Log_record.write_commit_log_record tx.log_manager tx.tx_num in
  Log_manager.flush tx.log_manager lsn;
  Transaction__Buffer_list.unpin_all ~buf_list:tx.buffers;
  Printf.printf "transaction %d committed" tx.tx_num
(* concurMgr.release() *)

module IntSet = Set.Make(struct
    type t = int
    let compare = compare
  end)
    
let run_recover tx =
  let finished_txs = ref IntSet.empty in
  let iter = Log_manager.get_iterator tx.log_manager in
  while Log_manager__Log_iterator.has_next iter do
    let bytes = Log_manager__Log_iterator.next iter in
    let record = Log_record.make ~byte:bytes in
    match record with
    | Commit r -> finished_txs := IntSet.add r.tx_num !finished_txs
    | Rollback r -> finished_txs := IntSet.add r.tx_num !finished_txs
    | Checkpoint -> ()
    | UpdateInt r -> if not (IntSet.mem r.tx_num (!finished_txs))
      then
        failwith "todo"
      else ()
    | _ -> failwith "todo"    
  done;
  failwith "todo"

let recover tx =
  Buffer_manager.flush_all tx.buffer_manager tx.tx_num;
  failwith "todo"

  
let rollback transaction = failwith "todo"

  Printf.printf "transaction %d committed" tx.tx_num
(* concurMgr.release() *)

let rollback tx =
  let log_iter = Log_manager.get_iterator tx.log_manager in
  let rec loop () =
    if Log_manager__Log_iterator.has_next log_iter then (
      let bytes = Log_manager__Log_iterator.next log_iter in
      let rollback_record = Log_record.make ~bytes in
      if rollback_record.tx_num != tx_num (failwith "todo")
        else (
      loop ())
     in
  loop();
  Buffer_manager.flush_all tx.buffer_manager tx.tx_num;
  let lsn = Log_record.write_rollback_log_record tx.log_manager tx.tx_num in
  Log_manager.flush tx.log_manager lsn;
  Transaction__Buffer_list.unpin_all tx.buffers;
  Printf.printf "transaction %d rolled back" tx.tx_num

let recover transaction = failwith "todo"
