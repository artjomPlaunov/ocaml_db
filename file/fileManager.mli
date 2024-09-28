type t

val make : string -> int -> t
val is_new : t -> bool
val get_blocksize : t -> int
val get_file : t -> string -> Unix.file_descr
val read : t -> BlockId.t -> Page.t -> unit
val write : t -> BlockId.t -> Page.t -> unit 
val append : t -> string -> BlockId.t
val size : t -> string -> int