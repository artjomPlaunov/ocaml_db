type t

val make : File.FileManager.t -> string -> t
val test : string -> string
val append_new_block : t -> File.BlockId.t
val append : t -> bytes -> int
