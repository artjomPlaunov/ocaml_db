open File

module IntSet = Set.Make (struct
  type t = int

  let compare = compare
end)

let next_tx_num = ref 0

let get_next_tx_num () =
  next_tx_num := !next_tx_num + 1;
  !next_tx_num

type t = {
  eof : int;
  buffer_manager : Buffer_manager.t;
  file_manager : File_manager.t;
  log_manager : Log_manager.t;
  tx_num : int;
  buffers : Transaction__Buffer_list.t;
}

let make ~file_manager ~log_manager ~buffer_manager =
  let tx_num = get_next_tx_num () in
  let _ = Log_record.write_start_log_record log_manager tx_num in
  {
    eof = -1;
    buffer_manager;
    file_manager;
    log_manager;
    tx_num;
    buffers = Buffer_list.make ~buffer_mgr:buffer_manager;
  }

let size ~tx ~filename =
  let block = File.Block_id.make ~filename ~block_num:tx.eof in
  File.File_manager.size tx.file_manager filename

let append ~tx ~filename =
  let block = File.Block_id.make ~filename ~block_num:tx.eof in
  File.File_manager.append tx.file_manager filename

let block_size ~tx = File.File_manager.get_blocksize tx.file_manager
let pin ~tx ~block = Transaction__Buffer_list.pin ~buf_list:tx.buffers ~block

let unpin ~tx ~block =
  Transaction__Buffer_list.unpin ~buf_list:tx.buffers ~block

let get_int32 ~tx ~block ~offset =
  let buf = Transaction__Buffer_list.get_buffer ~buf_list:tx.buffers ~block in
  let page = Buffer_manager__Db_buffer.contents buf in
  Page.get_int32 page offset

let get_string ~tx ~block ~offset =
  let buf = Transaction__Buffer_list.get_buffer ~buf_list:tx.buffers ~block in
  let page = Buffer_manager__Db_buffer.contents buf in
  Page.get_string page offset

let set_string ~tx ~block ~offset ~value ~to_log =
  (* concurrency manager lock *)
  let buf = Transaction__Buffer_list.get_buffer ~buf_list:tx.buffers ~block in
  let page = Buffer_manager__Db_buffer.contents buf in
  let lsn =
    if to_log then
      let block = Buffer_manager__Db_buffer.block buf in
      Log_record.write_update_string_log_record tx.log_manager tx.tx_num block
        offset value
    else -1
  in
  Page.set_string page offset value;
  Buffer_manager__Db_buffer.set_modified buf tx.tx_num lsn

let set_int ~tx ~block ~offset ~value ~to_log =
  let buffer =
    Transaction__Buffer_list.get_buffer ~buf_list:tx.buffers ~block
  in
  let page = Buffer_manager__Db_buffer.contents buffer in
  let lsn =
    if to_log then
      let block = Buffer_manager__Db_buffer.block buffer in
      Log_record.write_update_int_log_record tx.log_manager tx.tx_num block
        offset value
    else -1
  in
  Page.set_int32 page offset value;
  Buffer_manager__Db_buffer.set_modified buffer tx.tx_num lsn

let commit tx =
  Buffer_manager.flush_all tx.buffer_manager tx.tx_num;
  let lsn = Log_record.write_commit_log_record tx.log_manager tx.tx_num in
  Log_manager.flush tx.log_manager lsn;
  Transaction__Buffer_list.unpin_all ~buf_list:tx.buffers

let run_recover tx =
  let finished_txs = ref IntSet.empty in
  let iter = Log_manager.get_iterator tx.log_manager in
  while Log_manager__Log_iterator.has_next iter do
    let bytes = Log_manager__Log_iterator.next iter in
    let record = Log_record.make ~bytes in
    match record with
    | Commit r -> finished_txs := IntSet.add r.tx_num !finished_txs
    | Rollback r -> finished_txs := IntSet.add r.tx_num !finished_txs
    | Checkpoint -> ()
    | UpdateString r ->
        if not (IntSet.mem r.tx_num !finished_txs) then (
          pin ~tx ~block:r.block;
          set_string ~tx ~block:r.block ~offset:r.offset ~value:r.value
            ~to_log:false;
          unpin ~tx ~block:r.block)
        else ()
    | UpdateInt r ->
        if not (IntSet.mem r.tx_num !finished_txs) then (
          pin ~tx ~block:r.block;
          set_int ~tx ~block:r.block ~offset:r.offset ~value:r.value
            ~to_log:false;
          unpin ~tx ~block:r.block)
        else ()
    | _ -> failwith "todo"
  done

let recover tx =
  run_recover tx;
  Buffer_manager.flush_all tx.buffer_manager tx.tx_num;
  let lsn = Log_record.write_checkpoint_log_record tx.log_manager in
  Log_manager.flush tx.log_manager lsn

let rollback tx =
  let log_iter = Log_manager.get_iterator tx.log_manager in
  while Log_manager__Log_iterator.has_next log_iter do
    let bytes = Log_manager__Log_iterator.next log_iter in
    let rollback_record = Log_record.make ~bytes in
    match rollback_record with
    | Checkpoint -> ()
    | Start r -> ()
    | Commit r -> ()
    | UpdateInt r ->
        if r.tx_num = tx.tx_num then (
          pin ~tx ~block:r.block;
          let buffer =
            Buffer_list.get_buffer ~buf_list:tx.buffers ~block:r.block
          in
          let lsn = -1 in
          let page = Buffer_manager__Db_buffer.contents buffer in
          Page.set_int32 page r.offset r.value;
          Buffer_manager__Db_buffer.set_modified buffer tx.tx_num lsn;
          unpin ~tx ~block:r.block)
        else ()
    | UpdateString r ->
        if r.tx_num = tx.tx_num then (
          pin ~tx ~block:r.block;
          let buffer =
            Buffer_list.get_buffer ~buf_list:tx.buffers ~block:r.block
          in
          let lsn = -1 in
          let page = Buffer_manager__Db_buffer.contents buffer in
          Page.set_string page r.offset r.value;
          Buffer_manager__Db_buffer.set_modified buffer tx.tx_num lsn;
          unpin ~tx ~block:r.block)
        else ()
    | Rollback r -> ()
  done;

  Buffer_manager.flush_all tx.buffer_manager tx.tx_num;
  let lsn = Log_record.write_rollback_log_record tx.log_manager tx.tx_num in
  Log_manager.flush tx.log_manager lsn;

  Transaction__Buffer_list.unpin_all ~buf_list:tx.buffers;
  Printf.printf "transaction %d rolled back" tx.tx_num
