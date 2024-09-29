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

let next log_itr = failwith "TODO"
