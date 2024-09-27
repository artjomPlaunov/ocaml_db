open Unix

type t = {
     _db_dir	    : dir_handle;
     _db_dirname  : string;
     _blocksize	  : int;
   }

exception OpenDbDirError

let rec clean_temp_dirs db_dirname db_dir = 
  try 
    let _cur_file = readdir db_dir in
    if String.length _cur_file >= 4 && (String.sub _cur_file 0 4) = "temp"
    then
      let _ = Sys.remove (Filename.concat db_dirname _cur_file) in
      clean_temp_dirs db_dirname db_dir
    else
      clean_temp_dirs db_dirname db_dir
  with
  | End_of_file -> ()

(* File Manager constructor. *)
let make _db_dirname _blocksize =
  (* Create open file handler for DB directory *)
  let _db_dir =
    try
      let stat = stat _db_dirname in
      if stat.st_kind = Unix.S_DIR
      then
        opendir _db_dirname
      else
        raise OpenDbDirError
    with
    (* If it doesn't exist already, create it. *)
    | Unix_error (Unix.ENOENT,_,_) ->
      let _ = mkdir _db_dirname 0o755 in
      opendir _db_dirname
    | _ -> raise OpenDbDirError 
  in
  (* Remove leftover temporary tables. *)
  let _ = clean_temp_dirs _db_dirname _db_dir in
  let _ = rewinddir _db_dir in 
  { _db_dir; _db_dirname; _blocksize; }
  
let get_blocksize file_mgr = file_mgr._blocksize


let get_file file_mgr fname = 
  let full_path = Filename.concat file_mgr._db_dirname fname in 
  openfile full_path [O_RDWR; O_CREAT; O_SYNC] 0o640


