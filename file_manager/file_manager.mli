type t

module Block_id : sig 
    type t
    val file_name : t -> string
    val block_num : t -> int
    val make : filename:string -> block_num:int -> t
    val to_string : t -> string
    val eq : t -> t -> bool
end

module Page : sig 
    type t
    val make : block_size:int -> t
    val from_bytes : bytes -> t
    val get_int32 : t -> int -> int32
    val set_int32 : t -> int -> int32 -> unit
    val contents : t -> bytes
    val get_bytes : t -> int -> bytes
    val set_bytes : t -> int -> bytes -> unit
    val get_string : t -> int -> string
    val set_string_raw : t -> int -> string -> unit
    val get_string_raw : t -> int -> int -> string
    val set_string : t -> int -> string -> unit
    val max_len : int -> int
    val ( = ) : t -> t -> bool
    val zero_out : t -> unit

end 


(**  Create new file manager for directory db_dirname and specify block size. *)
val make : db_dirname:string -> block_size:int -> t
val is_new : t -> bool
val get_blocksize : t -> int
val get_file : t -> string -> Unix.file_descr
val read : t -> Block_id.t -> Page.t -> unit
val write : t -> Block_id.t -> Page.t -> unit
val append : t -> string -> Block_id.t
val size : t -> string -> int
