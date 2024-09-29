type t

val make : File.FileManager.t -> File.BlockId.t -> t

val has_next : t -> bool

val next : t -> bytes