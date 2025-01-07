open File

(* Note on block layout: 
  
  Block 0 contains metadata. 
  It only modifies the first 4 bytes, for the free list, and keeps other data consistent. 
  Programs using the storage manager can modify the remaining block space as necessary. 

  All the other blocks not currently in the free list are being 
  used.
*)
type t = {
  file_manager: File_manager.t;
  storage_file: string; 
  mutable head_page: Page.t;
}

let get_head_page ~storage_manager = storage_manager.head_page

let set_head_page ~storage_manager page =
  storage_manager.head_page <- page;
  let block_id = Block_id.make ~filename:storage_manager.storage_file ~block_num:0 in 
  File_manager.write storage_manager.file_manager block_id storage_manager.head_page

let make ~file_manager ~storage_file = 
  let block_size = File_manager.get_blocksize file_manager in 
  let head_page = Page.make ~block_size in 
  let block = Block_id.make ~filename:storage_file ~block_num:0 in 
  if File_manager.size file_manager storage_file = 0 then (
    Page.set_int32 head_page 0 (Int32.of_int 0);
    File_manager.write file_manager block head_page
  ) else
    File_manager.read file_manager block head_page;
  {file_manager; storage_file; head_page}

let append ~storage_manager ~page = 
  let fm = storage_manager.file_manager in 
  let block_size = File_manager.get_blocksize fm in 
  let sfile = storage_manager.storage_file in 
  let head_page = storage_manager.head_page in 
  let head_ptr = Block_id.make ~filename:sfile ~block_num:0 in
  let next_ptr = Int32.to_int (Page.get_int32 head_page 0) in 
  (* if next is 0, free list is empty so we append at end of file. *)
  if next_ptr = 0 then (
    let blocksize = File_manager.get_blocksize fm in 
    let block = File_manager.append fm sfile in 
    File_manager.write fm block page;
    block
  (* otherwise, we get a block from the free list*)
  ) else 
    (* Read first element from free list into a page*)
    let next_ptr = Block_id.make ~filename:sfile ~block_num:next_ptr in 
    let next_page = Page.make ~block_size in 
    File_manager.read fm next_ptr next_page;
    (* Save pointer from first element in list, and update head pointer.*)
    let next_next_ptr = Page.get_int32 next_page 0 in 
    Page.set_int32 head_page 0 next_next_ptr;
    File_manager.write fm head_ptr head_page;
    (* Finally, write append data to block we fetched from the freelist.*)
    File_manager.write fm next_ptr page;
    next_ptr

let delete ~storage_manager ~block = 
  let fm = storage_manager.file_manager in 
  let block_size = File_manager.get_blocksize fm in 
  let sfile = storage_manager.storage_file in 
  let head_page = storage_manager.head_page in 
  let head_ptr = Block_id.make ~filename:sfile ~block_num:0 in
  let next_ptr = Page.get_int32 head_page 0 in 
  let page = Page.make ~block_size in 
  Page.set_int32 page 0 next_ptr;
  Page.set_int32 head_page 0 (Int32.of_int (Block_id.block_num block));
  File_manager.write fm head_ptr head_page;
  File_manager.write fm block page

let update ~storage_manager ~block ~page = 
  let fm = storage_manager.file_manager in 
  let block_size = File_manager.get_blocksize fm in 
  let sfile = storage_manager.storage_file in 
  File_manager.write fm block page

let update_block_num ~storage_manager ~block_num ~page = 
  let block = Block_id.make ~filename:storage_manager.storage_file ~block_num in  
  update ~storage_manager ~block ~page

let get_block ~storage_manager ~block_num = 
  let block_size = File_manager.get_blocksize storage_manager.file_manager in 
  let page = Page.make ~block_size in 
  let block = Block_id.make ~filename:storage_manager.storage_file ~block_num in 
  File_manager.read storage_manager.file_manager block page;
  page
    


  


