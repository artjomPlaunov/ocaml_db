open File

type t

val make : file_manager:File_manager.t -> block:Block_id.t -> t
val has_next : t -> bool
val next : t -> bytes
