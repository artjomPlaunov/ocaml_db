open File

type t

val make : File_manager.t -> Block_id.t -> t
val has_next : t -> bool
val next : t -> bytes
