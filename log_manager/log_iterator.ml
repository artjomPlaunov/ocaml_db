open File

type t = {
  fm : File_manager.t;
  block : Block_id.t ref;
  page : Page.t;
  cur_pos : int ref;
  boundary : int ref;
}

let make fm b =
  let page = Page.make (File_manager.get_blocksize fm) in
  let _ = File_manager.read fm b page in
  let boundary = ref (Int32.to_int (Page.get_int32 page 0)) in
  let cur_pos = ref !boundary in
  let block = ref b in
  { fm; block; page; cur_pos; boundary }

let has_next log_itr =
  !(log_itr.cur_pos) < File_manager.get_blocksize log_itr.fm
  || Block_id.block_num !(log_itr.block) > 0

let move_to_block log_itr b =
  let _ = File_manager.read log_itr.fm b log_itr.page in
  let _ = log_itr.boundary := Int32.to_int (Page.get_int32 log_itr.page 0) in
  log_itr.cur_pos := !(log_itr.boundary)

let next log_itr =
  let blocksize = File_manager.get_blocksize log_itr.fm in
  let _ =
    if !(log_itr.cur_pos) = blocksize then
      let filename = Block_id.file_name !(log_itr.block) in
      let block_num = Block_id.block_num !(log_itr.block) in
      let _ =
        log_itr.block := Block_id.make ~filename ~block_num:(block_num - 1)
      in
      move_to_block log_itr !(log_itr.block)
    else ()
  in
  let record = Page.get_bytes log_itr.page !(log_itr.cur_pos) in
  let _ = log_itr.cur_pos := 4 + Bytes.length record in
  record
