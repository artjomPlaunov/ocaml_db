type t = {
     _db_dir	: Unix.dir_handle;
     _blocksize	: int;
   }

exception OpenDbDirError

let make db_dirname _blocksize =
  let _db_dir =
    try
      let stat = Unix.stat db_dirname in
      if stat.Unix.st_kind = Unix.S_DIR
      then
        Unix.opendir db_dirname
      else
        raise OpenDbDirError
    with
    | Unix.Unix_error (Unix.ENOENT,_,_) ->
      let _ = Unix.mkdir db_dirname 0o755 in
      Unix.opendir db_dirname
    | _ -> raise OpenDbDirError 
  in
  { _db_dir; _blocksize; }
  



