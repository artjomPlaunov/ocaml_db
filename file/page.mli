type t

val make : block_size:int -> t
val from_bytes : bytes -> t
val get_int32 : t -> int -> int32
val set_int32 : t -> int -> int32 -> unit
val contents : t -> bytes
val get_bytes : t -> int -> bytes
val set_bytes : t -> int -> bytes -> unit
val get_string : t -> int -> string
val set_string : t -> int -> string -> unit
val max_len : int -> int
val ( = ) : t -> t -> bool
val zero_out : t -> unit
