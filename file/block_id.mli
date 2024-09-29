type t

val file_name : t -> string
val block_num : t -> int
val make : filename:string -> block_num:int -> t
