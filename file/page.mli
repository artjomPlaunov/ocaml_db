type t
  
val make : int -> t

val get_int32 : t -> int -> int32

val set_int32 : t -> int -> int32 -> unit

val contents : t -> bytes

val get_bytes : t -> int -> bytes

val set_bytes : t -> int -> bytes -> unit

val get_string : t -> int -> string

val set_string : t -> int -> string -> unit

val max_len : int -> int
