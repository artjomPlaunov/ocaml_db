open File

type t

val make : File_manager.t -> string -> t
val test : string -> string
val append_new_block : t -> Block_id.t
val append : t -> bytes -> int
val flush : t -> int -> unit
