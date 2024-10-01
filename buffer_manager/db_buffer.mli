open File

type t

val make : file_manager:File_manager.t -> log_manager:Log_manager.t -> t
val contents : t -> Page.t
val block : t -> Block_id.t
val is_pinned : t -> bool
val is_unpinned : t -> bool
val pin : t -> unit
val unpin : t -> unit
val set_modified : t -> int -> int -> unit
val modifying_tx : t -> int
val flush : t -> unit
val assign_to_block : t -> Block_id.t -> unit
