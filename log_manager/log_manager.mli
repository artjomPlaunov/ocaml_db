open File

type t

val make : file_manager:File_manager.t -> log_file:string -> t
val test : string -> string
val append_new_block : t -> Block_id.t
val append : t -> bytes -> int
val flush : t -> int -> unit
