type t

val make : string -> int -> t

val get_blocksize : t -> int

val get_file : t -> string -> Unix.file_descr
