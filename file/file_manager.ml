type t = {
  _is_new : bool;
  _db_dir : Unix.dir_handle;
  db_dirname : string;
  block_size : int;
}

exception InitDbErr
exception FileMgrReadErr
(*exception NotImplemented*)

let rec clean_temp_dirs db_dirname db_dir =
  try
    let cur_file = Unix.readdir db_dir in
    if String.length cur_file >= 4 && String.sub cur_file 0 4 = "temp" then
      let _ = Sys.remove (Filename.concat db_dirname cur_file) in
      clean_temp_dirs db_dirname db_dir
    else clean_temp_dirs db_dirname db_dir
  with End_of_file -> ()

(* File Manager constructor. *)
let make ~db_dirname ~block_size =
  (* Create open file handler for DB directory *)
  let _db_dir, _is_new =
    try
      let stat = Unix.stat db_dirname in
      if stat.st_kind = Unix.S_DIR then (Unix.opendir db_dirname, false)
      else raise InitDbErr
    with
    (* If it doesn't exist already, create it. *)
    | Unix.Unix_error (Unix.ENOENT, _, _) ->
        let _ = Unix.mkdir db_dirname 0o755 in
        (Unix.opendir db_dirname, true)
    | _ -> raise InitDbErr
  in
  (* Remove leftover temporary tables. *)
  let _ = clean_temp_dirs db_dirname _db_dir in
  let _ = Unix.rewinddir _db_dir in
  { _is_new; _db_dir; db_dirname; block_size }

let is_new file_mgr = file_mgr._is_new
let get_blocksize file_mgr = file_mgr.block_size

let get_file file_mgr fname =
  let full_path = Filename.concat file_mgr.db_dirname fname in
  Unix.openfile full_path Unix.[ O_RDWR; O_CREAT; O_SYNC ] 0o755

let read file_mgr block page =
  let fd = get_file file_mgr (Block_id.file_name block) in
  let offset = Block_id.block_num block * file_mgr.block_size in
  let _ = Unix.lseek fd offset SEEK_SET in
  let n = Unix.read fd (Page.contents page) 0 file_mgr.block_size in
  if n <> file_mgr.block_size then raise FileMgrReadErr else ()

(* Since Unix.write doesn't guarantee writing all n bytes,
   we have a helper function to repeatedly call write until we
   have written all n bytes.

   Note there is a possible uncaught exception here, if anything
   goes wrong with writing.
*)
let rec write_n fd page offset n =
  if n = 0 then ()
  else
    let bytes_written = Unix.write fd page offset n in
    write_n fd page (offset + bytes_written) (n - bytes_written)

let write file_mgr block page =
  let fd = get_file file_mgr (Block_id.file_name block) in
  let offset = Block_id.block_num block * file_mgr.block_size in
  let _ = Unix.lseek fd offset SEEK_SET in
  write_n fd (Page.contents page) 0 file_mgr.block_size

let size file_mgr fname =
  let full_path = Filename.concat file_mgr.db_dirname fname in
  let stat = Unix.stat full_path in
  stat.st_size / file_mgr.block_size

let append file_mgr filename =
  let block_num = size file_mgr filename in
  let block = Block_id.make ~filename ~block_num in
  let b = Bytes.make file_mgr.block_size '\000' in
  let fd = get_file file_mgr filename in
  let _ = write_n fd b (block_num * file_mgr.block_size) file_mgr.block_size in
  block
