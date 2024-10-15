type t

val make : Schema.t -> t
val create : Schema.t -> (string, int) Hashtbl.t -> int -> t
val get_schema : t -> Schema.t
val get_offset : t -> string -> int
val get_slot_size : t -> int