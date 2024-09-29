open File

type t = {
  fm:       FileManager.t;
  block:    BlockId.t ref;
  page:     Page.t;
  cur_pos:  int ref;
  boundary: int ref;
}

let make fm b = 
  let page = Page.make (FileManager.get_blocksize fm) in 
  let _ = FileManager.read fm b page in 
  let boundary = ref (Int32.to_int (Page.get_int32 page 0)) in
  let cur_pos = ref !boundary in 
  let block = ref b in 
  {fm; block; page; cur_pos; boundary;}

let has_next log_itr = 
  (!(log_itr.cur_pos) < (FileManager.get_blocksize log_itr.fm))
|| ((BlockId.block_num !(log_itr.block)) > 0)

let next log_itr = failwith "TODO"