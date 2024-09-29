type t

val make : db_dirname:string -> block_size:int -> t
val is_new : t -> bool
val get_blocksize : t -> int
val get_file : t -> string -> Unix.file_descr
val read : t -> Block_id.t -> Page.t -> unit
val write : t -> Block_id.t -> Page.t -> unit
val append : t -> string -> Block_id.t
val size : t -> string -> int
