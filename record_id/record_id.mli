type t

val make : block_num:int -> slot:int -> t
val get_block_num : rid:t -> int
val get_slot : rid:t -> int

val to_string : rid:t -> string
