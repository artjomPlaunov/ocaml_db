open File
open LogManager

type t = {
  file_manager : File_manager.t;
  log_manager : Log_manager.t;
  contents : Page.t;
  mutable block : Block_id.t;
  mutable pins : int;
  mutable tx_num : int;
  mutable lsn : int;
}

let make ~file_manager ~log_manager =
  {
    file_manager;
    log_manager;
    contents = Page.make (File_manager.get_blocksize file_manager);
    block = Block_id.make ~filename:"" ~block_num:(-1);
    pins = 0;
    tx_num = -1;
    lsn = -1;
  }

let contents buffer = buffer.contents
let block buffer = buffer.block

let set_modified buffer tx_num lsn =
  buffer.tx_num <- tx_num;
  if buffer.lsn >= 0 then buffer.lsn <- lsn

let is_pinned buffer = buffer.pins > 0
let modifying_tx buffer = buffer.tx_num
let pin buffer = buffer.pins <- buffer.pins + 1
let unpin buffer = buffer.pins <- buffer.pins - 1

let flush buffer =
  if buffer.tx_num >= 0 then (
    Log_manager.flush buffer.log_manager buffer.lsn;
    File_manager.write buffer.file_manager buffer.block buffer.contents;
    unpin buffer)

let assign_to_block buffer block =
  flush buffer;
  File_manager.read buffer.file_manager block buffer.contents;
  buffer.pins <- 0
